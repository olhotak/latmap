package latmap

trait Rule {
  val headElement: RuleElement
  val bodyElements: List[RuleElement]
}

trait RuleElement {
  type Variable = AnyRef

  /** The set of variables that are bound after this rule element has been evaluated. */
  val variables: Set[Variable]

  /** The cost estimate of evaluating this rule element if the boundVars are already bound
    * and other variables are free.
    * MAX_INT means infinite cost. */
  def costEstimate(boundVars: Set[Variable]): Int

  /** Produces a PlanElement such that when boundVars are bound, executing this PlanElement
    * will make all variables in the `variables` set bound. regAlloc is the mapping of variables
    * to registers in the
    */
  def planElement(boundVars: Set[Variable], regAlloc: Variable=>Int): PlanElement

  // TODO: maybe we should define custom types for cost and for register numbers (not just use Int).
}
