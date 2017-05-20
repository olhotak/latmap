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
  def plan(rule: Rule, bodyIdx: Option[Int] = None): Plan = {
    // Step 1: Allocate variables to registers
    val var2reg = allocateVariables(rule)
    val boundVars = mutable.Set[Variable]()

    // skip step 2 for constant elements
    // Step 2: Create an initial PlanElement and add its variables to the bound list
    val initBodyRule : RuleElement = bodyIdx match {
      case Some(x) => rule.bodyElements(x)
      case None => rule.bodyElements(0)
    }

    val initPlanElement = initBodyRule match {
      case latConstRuleElement : LatConstantRuleElement =>
        latConstRuleElement.planElement(Set(), var2reg)
      case constRuleElement : KeyConstantRuleElement =>
        constRuleElement.planElement(Set(), var2reg)
      case latMapRuleElement : LatmapRuleElement =>
        latMapRuleElement.planElement(Set(), var2reg, Some(latmap.Input))
    }
    // create new InputRuleElement which just reads all facts from input latmap to do semi-naive evaluation

    boundVars ++= initBodyRule.variables
    var curPlanElement = initPlanElement

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
      // add new parameter to planElement() for indexCreation
      curPlanElement.next = best.planElement(boundVars.toSet, var2reg, Some(True))
      boundVars ++= best.variables
      curPlanElement = curPlanElement.next
      remaining.remove(best)
    }
    println("\n")

    // Step 4: Add a final PlanElement that writes the result to the LatMap
    val write = rule.headElement.writeToLatMap(var2reg)
    curPlanElement.next = write

    Plan(initPlanElement, rule)
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
