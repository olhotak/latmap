package latmap

trait Variable { val name: AnyRef }
case class KeyVariable(name: AnyRef) extends Variable
case class LatVariable(name: AnyRef) extends Variable

case class Rule(headElement: RuleElement,
                bodyElements: List[RuleElement]) {
  val variables: Seq[Variable] = headElement.variables ++ bodyElements.flatMap((be) => be.variables)
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
  // TODO: Add 'selectIndex' to LatMap for a given set of bound variables
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

  override def variables: Seq[Variable] = vars
  override def costEstimate(boundVars: Set[Variable]): Int = {
    0 // TODO
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    IndexScan(
      latmap.indexes(???),
      mergeLat = true,
      inputRegs = boundVars.map(regAlloc).toArray,
      outputRegs = boundVars.map(regAlloc).toArray,
      ??? // outputLatReg
    )
  }
}

/**
  * RuleElement for a rule representing a relation.
  */
class RelationRuleElement(vars: Seq[Variable]) extends RuleElement {
  assert(vars.forall(_.isInstanceOf[KeyVariable]))

  override def variables: Seq[Variable] = vars
  override def costEstimate(boundVars: Set[Variable]): Int = {
    0 // TODO
  }
  override def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement = {
    ???
  }
}
