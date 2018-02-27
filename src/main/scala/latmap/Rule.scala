package latmap

import scala.collection.mutable

trait Variable { val name: Int }
case class KeyVariable(name: Int) extends Variable {
  override def toString: String = s"K$name"
}
case class LatVariable(name: Int, lattice: Lattice) extends Variable {
  override def toString: String = s"L$name($lattice)"
}

case class Rule(headElement: LatmapRuleElement,
                bodyElements: List[RuleElement]) {
  val variables: Set[Variable] = (headElement.variables ++ bodyElements.flatMap((be) => be.variables)).toSet
  val numKeyVars: Int = variables.count(_.isInstanceOf[KeyVariable])
  val numLatVars: Int = variables.count(_.isInstanceOf[LatVariable])
}

/**
  * RuleElement. Anything that can be in the head or body of a Rule.
  */
trait RuleElement {
  def variables: Seq[Variable]

  /** The cost estimate of evaluating this rule element if the boundVars are already bound
    * and other variables are free.
    * MAX_INT means infinite cost. */
  // TODO: maybe we should define custom types for cost and for register numbers (not just use Int).
  def costEstimate(boundVars: Set[Variable]): Int

  /** Produces a PlanElement such that when boundVars are bound, executing this PlanElement
    * will make all variables in the `variables` set bound. regAlloc is the mapping of variables
    * to registers in the evaluation context.
    */
  def planElements(boundVars: Set[Variable], regAlloc: RegAlloc): Seq[PlanElement]
}

/**
  * RuleElement for a rule involving a single lattice element. e.g. Dist(a, b, d)
  * @param latmapGroup The LatMapGroup of the lattice element.
  * @param vars The variables in the body of the rule.
  */
class LatmapRuleElement(val latmapGroup: LatMapGroup, vars: Seq[Variable], constRule : Boolean) extends RuleElement {
  private val keyVars = vars.collect{case kv: KeyVariable => kv}
  private val latVars = vars.collect{case lv: LatVariable => lv}
  assert(latVars.size == 1)
  private val latVar = latVars.head

  override def variables: Seq[Variable] = vars
  def findIndex(boundVars: Set[Variable]): Index = {
    val boundKeys = keyVars.toSet.intersect(boundVars)
    latmapGroup.trueLatMap.selectIndex(boundKeys.map(vars.indexOf(_)))
  }
  override def costEstimate(boundVars: Set[Variable]): Int = {
    val index = findIndex(boundVars)
    (index match {
      case _: NaiveIndex => 1000
      case _: BoolAllKeyIndex => 0
      case _: GeneralAllKeyIndex => 0
      case _: HashIndex => 100
    }) + keyVars.filterNot(boundVars.contains(_)).size
  }

  def inputPlanElement(regAlloc: RegAlloc): PlanElement = {
    if(latmapGroup.lattice == BoolLattice)
      new BoolInputPlanElement(keyVars.map(regAlloc.keyRegs).toArray, latmapGroup)
    else
      new GeneralInputPlanElement(keyVars.map(regAlloc.keyRegs).toArray, regAlloc.latRegs(latVar), latmapGroup)
  }

  override def planElements(boundVars: Set[Variable], regAlloc: RegAlloc): Seq[PlanElement] = {
    val index = findIndex(boundVars)
    val inputRegs = keyVars.filter(boundVars).map(regAlloc.keyRegs).toArray
    val outputRegs = keyVars.filterNot(boundVars).map(regAlloc.keyRegs).toArray
    if(latmapGroup.lattice == BoolLattice)
      Seq(new BoolIndexScan(index, inputRegs, outputRegs))
    else {
      def scan(outputLatReg: Int) = new GeneralIndexScan(index, inputRegs, outputRegs, outputLatReg)
      if(boundVars.contains(latVar)) {
        val outputLatReg = regAlloc.freshLatReg()
        Seq(
          scan(outputLatReg),
          new GlbPlanElement(regAlloc.latRegs(latVar), outputLatReg, latVar.lattice)
        )
      } else {
        Seq(scan(regAlloc.latRegs(latVar)))
      }
    }
  }

  def writeToLatMap(regAlloc: RegAlloc): PlanElement = {
    if(latmapGroup.lattice == BoolLattice)
      new WriteToBoolLatMap(keyVars.map(regAlloc.keyRegs).toArray, latmapGroup)
    else
      new WriteToGeneralLatMap(keyVars.map(regAlloc.keyRegs).toArray, regAlloc.latRegs(latVar), latmapGroup)
  }

  override def toString: String = s"$latmapGroup(${variables.mkString(", ")})"
}


/**
  * RuleElement for key constants
  * @param keyVar The variable in the body of the rule.
  * @param const value of keyVar
  */
class KeyConstantRuleElement(keyVar: KeyVariable, const : Any) extends RuleElement {
  override def variables: Seq[Variable] = Seq(keyVar)
  override def costEstimate(boundVars: Set[Variable]): Int = 0
  override def planElements(boundVars: Set[Variable], regAlloc: RegAlloc): Seq[PlanElement] = {
    if (boundVars.contains(keyVar)) Seq(new KeyConstantFilter(regAlloc.keyRegs(keyVar), const))
    else Seq(new KeyConstantEval(regAlloc.keyRegs(keyVar), const))
  }

  override def toString: String = s"$keyVar := $const"
}

class LatConstantRuleElement(latVar: LatVariable, const : Any, lattice : Lattice) extends RuleElement {
  override def variables: Seq[Variable] = Seq(latVar)
  override def costEstimate(boundVars: Set[Variable]): Int = 0
  override def planElements(boundVars: Set[Variable], regAlloc: RegAlloc): Seq[PlanElement] = {
    if(const == lattice.bottom) Seq(new DeadEnd())
    else if (lattice == BoolLattice) Seq()
    else {
      if(boundVars.contains(latVar)) {
        val outputLatReg = regAlloc.freshLatReg()
        Seq(
          new LatConstantEval(outputLatReg, const),
          new GlbPlanElement(regAlloc.latRegs(latVar), outputLatReg, latVar.lattice)
        )
      } else {
        Seq(new LatConstantEval(regAlloc.latRegs(latVar), const))
      }
    }
  }

  override def toString: String = s"$latVar := $const"
}
/**
  * RuleElement for a transfer function.
  */
class FunctionRuleElement(function: AnyRef, vars: Seq[Variable], outputVar: Variable)
  extends RuleElement {
  override def variables: Seq[Variable] = vars :+ outputVar
  override def costEstimate(boundVars: Set[Variable]): Int = {
    if (vars.toSet.subsetOf(boundVars)) 0 else Int.MaxValue
  }
  override def planElements(boundVars: Set[Variable], regAlloc: RegAlloc): Seq[PlanElement] = {
    assert(vars.toSet.subsetOf(boundVars))
    val ret = new mutable.ArrayBuffer[PlanElement]()
    val regs = new mutable.ArrayBuffer[Int]()
    for(v <- vars) {
      v match {
        case kv: KeyVariable =>
          val reg = regAlloc.freshLatReg()
          ret.append(new CastKeyLat(reg, regAlloc.keyRegs(kv)))
          regs.append(reg)
        case lv: LatVariable => regs.append(regAlloc.latRegs(lv))
      }
    }
    val outputReg = outputVar match {
      case k: KeyVariable => regAlloc.freshLatReg()
      case l: LatVariable => regAlloc.latRegs(l)
    }
    ret.append(new Function(regs.toArray, outputReg, function))
    outputVar match {
      case k: KeyVariable => ret.append(new CastLatKey(regAlloc.keyRegs(k), outputReg))
      case l: LatVariable => ret.append(new CheckBottom(outputReg, l.lattice))
    }
    ret
  }
  override def toString: String = "Function"
}
