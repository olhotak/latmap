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

    val remaining = mutable.Set(rule.bodyElements:_*)
    var curPlanElement: Option[PlanElement] = None
    var initPlanElement: Option[PlanElement] = curPlanElement

    def addPlanElement(pe: PlanElement, re: RuleElement): Unit = {
      curPlanElement match {
        case Some(cur) =>
          cur.next = pe
        case None =>
          initPlanElement = Some(pe)
      }
      curPlanElement = Some(pe)

      boundVars ++= re.variables
      remaining.remove(re)
    }

    bodyIdx match {
      case Some(x) =>
        val be = rule.bodyElements(x)
        assert(be.isInstanceOf[LatmapRuleElement])

        addPlanElement(be.planElement(Set(), var2reg, Some(latmap.Input)), be)
      case None =>
    }

    // create new InputRuleElement which just reads all facts from input latmap to do semi-naive evaluation

    // Step 3: Greedily add the lowest cost PlanElement to the plan
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
      addPlanElement(best.planElement(boundVars.toSet, var2reg, Some(True)), best)

    }
    //println("\n")

    // Step 4: Add a final PlanElement that writes the result to the LatMap
    val write = rule.headElement.writeToLatMap(var2reg)
    curPlanElement.get.next = write

    Plan(initPlanElement.get, rule)
  }

  def allocateVariables(rule: Rule): mutable.Map[Variable, Int] = {
    val var2reg = new mutable.HashMap[Variable, Int]
    var numKeyVars = 0
    var numLatVars = 0

    rule.variables.foreach((v) => v match {
      case KeyVariable(keyVar) =>
        var2reg(v) = numKeyVars
        numKeyVars += 1
      case LatVariable(latVar, lattice) =>
        if (lattice != BoolLattice) {
          var2reg(v) = numLatVars + 1000
        } else {
          var2reg(v) = -1
        }
        numLatVars += 1
    })

    assert(numKeyVars == rule.numKeyVars)
    assert(numLatVars == rule.numLatVars)
    var2reg
  }
}
