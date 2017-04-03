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
    val headLat = new SimpleLatMap(lattice, 2)
    val planner = new Planner()
    val a = KeyVariable("a")
    val b = KeyVariable("b")
    val c = KeyVariable("c")
    val d1 = LatVariable("d1")
    val d2 = LatVariable("d2")
    val d1plusd2 = LatVariable("d1+d2")
    val rule = Rule(
      new LatmapRuleElement(headLat, Seq(a, c, d1plusd2)),
      List(
        new LatmapRuleElement(headLat, Seq(a, b, d1)),
        new LatmapRuleElement(headLat, Seq(b, c, d2)),
//        new TransferFnRuleElement((a: Array[lattice.Elem]) => {
//          (a(0), a(1)) match {
//            case (DistLattice.Infinity, _) => DistLattice.Infinity
//            case (_, DistLattice.Infinity) => DistLattice.Infinity
//            case (DistLattice.Dst(n1), DistLattice.Dst(n2)) => DistLattice.Dst(n1 + n2)
//            case _ => DistLattice.NegInfinity
//          }
//        },
        new TransferFnRuleElement((a: Array[Any]) => {
          (a(0), a(1)) match {
            case (DistLattice.Infinity, _) => DistLattice.Infinity
            case (_, DistLattice.Infinity) => DistLattice.Infinity
            case (DistLattice.Dst(n1), DistLattice.Dst(n2)) => DistLattice.Dst(n1 + n2)
            case _ => DistLattice.NegInfinity
          }
        },
          Seq(d1, d2), d1plusd2)
      )
    )
    // Create a plan starting with the ShortestDist(a, b, d1) body element
    val (plan, evalContext) = planner.plan(rule, 0)
    plan.planElements.go(evalContext)
  }
}
