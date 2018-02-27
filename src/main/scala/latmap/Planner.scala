package latmap

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class RegAlloc(rule: Rule) {
  var numKeyRegs = 0
  var numLatRegs = 0
  def freshKeyReg(): Int = {
    val ret = numKeyRegs
    numKeyRegs += 1
    ret
  }
  def freshLatReg(): Int = {
    val ret = numLatRegs
    numLatRegs += 1
    ret
  }
  var keyRegs = Map[KeyVariable, Int]()
  var latRegs = Map[LatVariable, Int]()

  for(v <- rule.variables) {
    v match {
      case kv: KeyVariable => keyRegs += (kv -> freshKeyReg())
      case lv: LatVariable => latRegs += (lv -> freshLatReg())
    }
  }
}

class Planner(program: Program) {
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
    val regAlloc = new RegAlloc(rule)
    var boundVars = Set[Variable]()

    // skip step 2 for constant elements
    // Step 2: Create an initial PlanElement and add its variables to the bound list

    val remaining = mutable.Set(rule.bodyElements:_*)
    val planElements = new ArrayBuffer[PlanElement]()

    def doneRuleElement(re: RuleElement): Unit = {
      boundVars ++= re.variables
      remaining.remove(re)
    }

    bodyIdx match {
      case Some(x) =>
        val re = rule.bodyElements(x).asInstanceOf[LatmapRuleElement]
        planElements.append(re.inputPlanElement(regAlloc))
        doneRuleElement(re)
      case None =>
    }

    // create new InputRuleElement which just reads all facts from input latmap to do semi-naive evaluation

    // Step 3: Greedily add the lowest cost PlanElement to the plan
    while (remaining.nonEmpty) {
      var best: RuleElement = null
      var bestCost = Int.MaxValue
      for (elem <- remaining) {
        val elemCost = elem.costEstimate(boundVars.toSet)
        if (elemCost <= bestCost) {
          bestCost = elemCost
          best = elem
        }
      }
      // add new parameter to planElement() for indexCreation
      planElements.appendAll(best.planElements(boundVars, regAlloc))
      doneRuleElement(best)

    }

    // Step 4: Add a final PlanElement that writes the result to the LatMap
    planElements.append(rule.headElement.writeToLatMap(regAlloc))

    if(planElements.exists(_.isInstanceOf[DeadEnd])) {
      planElements.clear()
      planElements.append(new DeadEnd())
    }

    for(pair <- planElements.sliding(2) if pair.size == 2) {
      pair(0).next = pair(1)
    }

    new Plan(planElements.head, rule, program.translator, regAlloc.numKeyRegs, regAlloc.numLatRegs)
  }
}
