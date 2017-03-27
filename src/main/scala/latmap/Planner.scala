package latmap

import latmap.RuleElement.{KeyVariable, LatVariable}

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
    val boundVars = mutable.Set[RuleElement.Variable]()
    val initBodyRule = rule.bodyElements(bodyIdx)

    // Step 2: Bind existing variables using existing facts
    val context = new EvalContext {
      override val latRegs: Array[Any] = new Array[Any](rule.numLatVars)
      override val translator: Translator = new Translator() // TODO: potentially reuse translator between plan calls?
      override val keyRegs: Array[Int] = new Array[Int](rule.numKeyVars)
    }

    var curPlanElement: PlanElement = initBodyRule.planElement(Set(), var2reg)
    boundVars ++= initBodyRule.variables

    // TODO: Take cost into account, instead of just adding plan elements in order
    for (be <- rule.bodyElements) {
      if (be != initBodyRule) {
        val newPlanElement = be.planElement(boundVars.toSet, var2reg) // TODO: not .toSet
        curPlanElement.next = newPlanElement
        curPlanElement = newPlanElement
      }
    }

    curPlanElement
  }

  def allocateVariables(rule: Rule): mutable.Map[RuleElement.Variable, Int] = {
    val var2reg = new mutable.HashMap[RuleElement.Variable, Int]
    var numKeyVars = 0
    var numLatVars = 0

    rule.variables.foreach((v) => v match {
      case KeyVariable(keyVar) => {
        var2reg(v) = numKeyVars
        numKeyVars += 1
      }
      case LatVariable(latVar) => {
        var2reg(v) = numLatVars + 1000
        numLatVars += 1
      }
    })

    assert(numKeyVars == rule.numKeyVars)
    assert(numLatVars == rule.numLatVars)
    var2reg
  }
}
