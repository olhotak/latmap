package latmap

trait Variable { val name: AnyRef }
case class KeyVariable(name: AnyRef) extends Variable
case class LatVariable(name: AnyRef, lattice: Lattice) extends Variable

case class Rule(headElement: RuleElement,
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
  def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement

  /** Produces 2 WriteToLatMap PlanElements for when this rule element is the head of a rule
    */
  def writeToLatMap(regAlloc: Variable=>Int): PlanElement = {
    throw new Exception("Used non-head RuleElement in the head position")
  }

}

/**
  * RuleElement for a rule involving a single lattice element. e.g. Dist(a, b, d)
  * @param latmapGroup The LatMapGroup of the lattice element.
  * @param vars The variables in the body of the rule.
  */
class LatmapRuleElement(val latmapGroup: LatMapGroup, vars: Seq[Variable], constRule : Boolean) extends RuleElement {
  private val keyVars = vars.filter(_.isInstanceOf[KeyVariable])
  private val latVars = vars.filter(_.isInstanceOf[LatVariable])
  assert(latVars.size == 1)
  private val latVar = latVars.head

  override def variables: Seq[Variable] = vars
  def findIndex(boundVars: Set[Variable]): Index = {
    val boundKeys = keyVars.toSet.intersect(boundVars)
    latmapGroup.trueLatMap.selectIndex(boundKeys.map(vars.indexOf(_)))
  }
  override def costEstimate(boundVars: Set[Variable]): Int = {
    val index = findIndex(boundVars)
    var cost = if(index.isInstanceOf[NaiveIndex]) 1000 else 100
    cost += keyVars.filterNot(boundVars.contains(_)).size
    cost
  }

  def inputPlanElement(regAlloc: Variable=>Int): PlanElement = {
    InputPlanElement(keyVars.map(regAlloc).toArray, regAlloc(latVar), latmapGroup)
  }

  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    latmapGroup.trueLatMap.lattice match {
      case BoolLattice =>
        BoolIndexScan(
          findIndex(boundVars),
          inputRegs = keyVars.map(regAlloc).toArray,
          outputRegs = keyVars.map(regAlloc).toArray
        )
      case _ =>
        IndexScan(
          //latmap.selectIndex(vars.zipWithIndex.collect { case (e, i) if boundVars.contains(e) => i }.toSet),
          latmapGroup.trueLatMap.selectIndex(boundVars.intersect(keyVars.toSet).map(vars.indexOf(_))),
          mergeLat = boundVars.contains(latVar) && !(latVar.asInstanceOf[LatVariable].lattice == BoolLattice),
          inputRegs = keyVars.map(regAlloc).toArray,
          outputRegs = keyVars.map(regAlloc).toArray,
          outputLatReg = regAlloc(latVar)
        )
    }
  }
  override def writeToLatMap(regAlloc: Variable=>Int): PlanElement = {
      WriteToLatMap(
        keyVars.map(regAlloc).toArray,
        regAlloc(latVar),
        latmapGroup,
        constRule
      )
  }
}


/**
  * RuleElement for key constants
  * @param keyVar The variable in the body of the rule.
  * @param const value of keyVar
  */
class KeyConstantRuleElement(keyVar: Variable, const : Any) extends RuleElement {

  override def variables: Seq[Variable] = Seq(keyVar)
  override def costEstimate(boundVars: Set[Variable]): Int = {
    0
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    if (boundVars.contains(keyVar))
      KeyConstantFilter(
       regAlloc(keyVar),
        const
      )
    else
      KeyConstantEval(
        regAlloc(keyVar),
        const
      )
  }

}

class LatConstantRuleElement(latVar: LatVariable, const : Any, lattice : Lattice) extends RuleElement {

  override def variables: Seq[Variable] = Seq(latVar)
  override def costEstimate(boundVars: Set[Variable]): Int = {
    0 // TODO: good cost estimate
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    // TODO: split up, using boundvars
    if (lattice == BoolLattice) {
      if (const == true) {
        NoOp()
      } else {
        DeadEnd()
      }
    } else {
      if (boundVars.contains(latVar))
        LatConstantFilter(
          regAlloc(latVar),
          const,
          latVar.lattice
        )
      else
        LatConstantEval(
          regAlloc(latVar),
          const,
          latVar.lattice
        )
    }
  }

}
/**
  * RuleElement for a rule representing a relation.
  */
/*
class RelationRuleElement(latmap: LatMap[_], vars: Seq[Variable]) extends RuleElement {
  assert(vars.forall(_.isInstanceOf[KeyVariable]))

  override def variables: Seq[Variable] = vars
  override def costEstimate(boundVars: Set[Variable]): Int = {
    0 // TODO: good cost estimate
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    KeyScan(
      latmap.selectIndex(boundVars.map(vars.indexOf(_)).filter(_ >= 0)),
      boundVars.map(regAlloc).toArray,
      boundVars.map(regAlloc).toArray
    )
  }

}
*/
class FilterFnRuleElement(function: AnyRef, vars: Seq[Variable])
  extends RuleElement {
  override def variables: Seq[Variable] = vars
  override def costEstimate(boundVars: Set[Variable]): Int = {
    if (vars.toSet.subsetOf(boundVars)) 0 else Int.MaxValue
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    assert(vars.toSet.subsetOf(boundVars))
    FilterFn(vars.map(regAlloc).toArray, function)
  }
}
/**
  * RuleElement for a transfer function.
  */
class TransferFnRuleElement(function: AnyRef, vars: Seq[Variable], outputVar: Variable)
  extends RuleElement {
  override def variables: Seq[Variable] = vars :+ outputVar
  override def costEstimate(boundVars: Set[Variable]): Int = {
    if (vars.toSet.subsetOf(boundVars)) 0 else Int.MaxValue
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    assert(vars.toSet.subsetOf(boundVars))
    val lattice = outputVar match {
      case k: KeyVariable => None
      case l: LatVariable => Some(l.lattice)
    }
    TransferFn(vars.map(regAlloc).toArray, regAlloc(outputVar), function, lattice)
  }
}
