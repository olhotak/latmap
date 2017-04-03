package latmap

import scala.collection.mutable

class Planner {
  /**
    * Planner steps:
    * - allocate variables to registers
    * - figure out which variables are bound initially
    * - repeat:
    * -- choose an unprocessed rule element from the body (greedily the one with lowest cost)
    * -- add its plan element to the plan
    * -- add its variables to the set of known bound variables
    * - until all body rule elements have been processed
    * - add plan element of head rule element to the plan
    *
    * @param rule: The rule to evaluate and obtain new facts from.
    * @param bodyIdx: The index of the initially body element in the rule.
    *
    */
  def plan(rule: Rule, bodyIdx: Int): (Plan, EvalContext) = {
    // Step 1: Allocate variables to registers
    val var2reg = allocateVariables(rule)
    val boundVars = mutable.Set[Variable]()
    val initBodyRule = rule.bodyElements(bodyIdx)

    // Step 2: Bind existing variables using existing facts
    val context = new EvalContext {
      override val keyRegs: Array[Int] = new Array[Int](rule.numKeyVars)
      override val latRegs: Array[Any] = new Array[Any](rule.numLatVars)
      override val translator: Translator = new Translator() // TODO: potentially reuse translator between plan calls?
    }

    val initPlanElement = initBodyRule.planElement(Set(), var2reg)
    boundVars ++= initBodyRule.variables
    var curPlanElement = initPlanElement

    // TODO: Take cost into account (esp. infinite costs), instead of just adding plan elements in order
    val remaining = mutable.Set(rule.bodyElements:_*) // :_* splats the array into a list of arguments
    while (remaining.nonEmpty) {
      var best: RuleElement = null
      var bestCost = Int.MaxValue
      for (elem <- remaining) {
        val curCost = elem.costEstimate(boundVars.toSet)
        if (curCost <= bestCost) {
          bestCost = curCost
          best = elem
        }
      }
      curPlanElement.next = best.planElement(boundVars.toSet, var2reg) // TODO: don't use .toSet?
      curPlanElement = curPlanElement.next
      remaining.remove(best)
    }

    curPlanElement.next = rule.headElement.writeToLatMap(var2reg)

    (Plan(initPlanElement), context)
  }

  def allocateVariables(rule: Rule): mutable.Map[Variable, Int] = {
    val var2reg = new mutable.HashMap[Variable, Int]
    var numKeyVars = 0
    var numLatVars = 0

    rule.variables.foreach((v) => v match {
      case KeyVariable(keyVar) =>
        var2reg(v) = numKeyVars
        numKeyVars += 1
      case LatVariable(latVar) =>
        var2reg(v) = numLatVars + 1000
        numLatVars += 1
    })

    assert(numKeyVars == rule.numKeyVars)
    assert(numLatVars == rule.numLatVars)
    var2reg
  }
}
