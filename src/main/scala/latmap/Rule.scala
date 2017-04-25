package latmap

trait Variable { val name: AnyRef }
case class KeyVariable(name: AnyRef) extends Variable
case class LatVariable(name: AnyRef) extends Variable

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

  /** Produces a WriteToLatMap PlanElement for when this rule element is the head of a rule.
    */
  def writeToLatMap(regAlloc: Variable=>Int): PlanElement = {
    throw new Exception("Used non-head RuleElement in the head position")
  }
}

/**
  * RuleElement for a rule involving a single lattice element. e.g. Dist(a, b, d)
  * @param latmap The LatMap of the lattice element.
  * @param vars The variables in the body of the rule.
  */
class LatmapRuleElement[T <: Lattice](latmap: LatMap[T], vars: Seq[Variable]) extends RuleElement {
  private val keyVars = vars.filter(_.isInstanceOf[KeyVariable])
  private val latVars = vars.filter(_.isInstanceOf[LatVariable])
  assert(latVars.size == 1)
  private val latVar = latVars.head

  override def variables: Seq[Variable] = vars
  override def costEstimate(boundVars: Set[Variable]): Int = {
    0 // TODO: good cost estimate
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    IndexScan(
      latmap.selectIndex(boundVars.map(vars.indexOf(_))),
      mergeLat = false,
      inputRegs = keyVars.map(regAlloc).toArray,
      outputRegs = keyVars.map(regAlloc).toArray,
      outputLatReg = regAlloc(latVar)
    )
  }
  override def writeToLatMap(regAlloc: Variable=>Int): PlanElement = {
    WriteToLatMap(
      keyVars.map(regAlloc).toArray,
      regAlloc(latVar),
      latmap
    )
  }
}

/**
  * RuleElement for a rule representing a relation.
  */
class RelationRuleElement(latmap: LatMap[_], vars: Seq[Variable]) extends RuleElement {
  assert(vars.forall(_.isInstanceOf[KeyVariable]))

  override def variables: Seq[Variable] = vars
  override def costEstimate(boundVars: Set[Variable]): Int = {
    0 // TODO: good cost estimate
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    KeyScan(
      latmap.selectIndex(boundVars.map(vars.indexOf(_))),
      boundVars.map(regAlloc).toArray,
      boundVars.map(regAlloc).toArray
    )
  }
}

/**
  * RuleElement for a transfer function.
  */
class TransferFnRuleElement(function: Array[Any] => Any, vars: Seq[Variable], outputVar: Variable)
  extends RuleElement {
//  private val liftedFn = (a: Array[Any]) => {
//    // TODO: This removes the need for casting in the public interface. But there may be a better way.
//    function(a.asInstanceOf[Array[T]])
//  }
  override def variables: Seq[Variable] = vars :+ outputVar
  override def costEstimate(boundVars: Set[Variable]): Int = {
    if (boundVars.subsetOf(vars.toSet)) 0 else Int.MaxValue
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    TransferFnArray(vars.map(regAlloc).toArray, regAlloc(outputVar), function)
  }
}