package latmap

case class Rule(headElement: RuleElement,
                bodyElements: List[RuleElement]) {
  val variables: Set[RuleElement.Variable] = headElement.variables ++ bodyElements.flatMap((be) => be.variables)
  val numKeyVars: Int = variables.count(_.isInstanceOf[RuleElement.KeyVariable])
  val numLatVars: Int = variables.count(_.isInstanceOf[RuleElement.LatVariable])
}

/**
  * Variable: can be a key variable or a lattice variable.
  */
object RuleElement {
  // TODO: Look at this
  trait Variable {
    val name: AnyRef
  }
  case class KeyVariable(name: AnyRef) extends Variable
  case class LatVariable(name: AnyRef) extends Variable
}

/**
  * RuleElement. Anything that can be in the head or body of a Rule.
  */
trait RuleElement {
  import RuleElement.Variable

  def variables: Set[Variable]

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
}
