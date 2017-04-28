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
  def plan(rule: Rule, bodyIdx: Int): Plan = {
    // Step 1: Allocate variables to registers
    val var2reg = allocateVariables(rule)
    val boundVars = mutable.Set[Variable]()
    val initBodyRule = rule.bodyElements(bodyIdx)

    // Step 2: Create an initial PlanElement and bind its variables
    val initPlanElement = initBodyRule.planElement(Set(), var2reg)
    boundVars ++= initBodyRule.variables
    var curPlanElement = initPlanElement
    println(curPlanElement)

    // Step 3: Greedily add the lowest cost PlanElement to the plan
    val remaining = mutable.Set(rule.bodyElements:_*) - initBodyRule
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
      curPlanElement.next = best.planElement(boundVars.toSet, var2reg)
      curPlanElement = curPlanElement.next
      println(curPlanElement)
      remaining.remove(best)
    }

    // Step 4: Add a final PlanElement that writes the result to the LatMap
    curPlanElement.next = rule.headElement.writeToLatMap(var2reg)

    Plan(initPlanElement)
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
    println(var2reg)
    var2reg
  }
}
