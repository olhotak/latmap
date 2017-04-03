package latmap

import org.scalatest.FunSuite

class PlannerTest extends FunSuite {
  test("allocate variables") {
    val lattice = DistLattice
    val latMap = new SimpleLatMap(lattice, 2)
    val outputLatMap = new SimpleLatMap(lattice, 2)
    val index: Index = new NaiveIndex(latMap, Set(1))
    val myTranslator: Translator = new Translator()

    val planner = new Planner()
    val rule = Rule(
      new RuleElement {
        override def costEstimate(boundVars: Set[Variable]) = Int.MaxValue
        override def variables = Seq(KeyVariable("a"), KeyVariable("c"), LatVariable("d"))
        override def planElement(boundVars: Set[Variable], regAlloc: (Variable) => Int) = {
          IndexScan(index, mergeLat = false, Array(0, 1), Array(0, 1), 0)
        }
      },
      List(
        // TODO: Fix placeholder planElement methods
        new RuleElement {
          override def costEstimate(boundVars: Set[Variable]) = Int.MaxValue
          override def variables = Seq(KeyVariable("a"), KeyVariable("b"), LatVariable("d1"))
          override def planElement(boundVars: Set[Variable], regAlloc: (Variable) => Int) = {
            IndexScan(index, mergeLat = false, Array(0, 1), Array(0, 1), 0)
          }
        },
        new RuleElement {
          override def costEstimate(boundVars: Set[Variable]) = Int.MaxValue
          override def variables = Seq(KeyVariable("b"), KeyVariable("c"), LatVariable("d2"))
          override def planElement(boundVars: Set[Variable], regAlloc: (Variable) => Int) = {
            IndexScan(index, mergeLat = false, Array(0, 1), Array(0, 1), 0)
          }
        }
      )
    )
    val var2reg = planner.allocateVariables(rule)
    println(s"Register allocation: ${var2reg}")
    println(s"Number of key variables: ${rule.numKeyVars}")
    println(s"Number of lattice variables: ${rule.numLatVars}")
  }
  test("Planner basic functionality") {
    val lattice = DistLattice
    val planner = new Planner()
    val rules = ???
  }
}
