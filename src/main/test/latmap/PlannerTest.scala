package latmap

import latmap.DistLattice.Dst
import org.scalatest.FunSuite

class PlannerTest extends FunSuite {
  test("planner allocates variables") {
    /**
      * Creates a Rule that has six variables, and asserts that
      * the planner allocates all six variables to different registers.
      */
    val lattice = DistLattice
    val latMap = new SimpleLatMap(lattice, 2)
    val index: Index = new NaiveIndex(latMap, Set(1))

    val a = KeyVariable("a")
    val b = KeyVariable("b")
    val c = KeyVariable("c")
    val d1plusd2 = KeyVariable("d1+d2")
    val d1 = KeyVariable("d1")
    val d2 = KeyVariable("d2")

    // TODO: Use existing RuleElements instead of creating ones inside this test?
    /**
      * Dist(a, c, d1plusd2) :- Dist(a, b, d1), Dist(b, c, d2).
      */
    val rule = Rule(
      new RuleElement {
        override def costEstimate(boundVars: Set[Variable]) = Int.MaxValue
        override def variables = Seq(a, c, d1plusd2)
        override def planElement(boundVars: Set[Variable], regAlloc: (Variable) => Int) = {
          IndexScan(index, mergeLat = false, Array(0, 1), Array(0, 1), 0)
        }
      },
      List(
        new RuleElement {
          override def costEstimate(boundVars: Set[Variable]) = Int.MaxValue
          override def variables = Seq(a, b, d1)
          override def planElement(boundVars: Set[Variable], regAlloc: (Variable) => Int) = {
            IndexScan(index, mergeLat = false, Array(0, 1), Array(0, 1), 0)
          }
        },
        new RuleElement {
          override def costEstimate(boundVars: Set[Variable]) = Int.MaxValue
          override def variables = Seq(b, c, d2)
          override def planElement(boundVars: Set[Variable], regAlloc: (Variable) => Int) = {
            IndexScan(index, mergeLat = false, Array(0, 1), Array(0, 1), 0)
          }
        }
      )
    )

    val planner = new Planner()
    val var2reg = planner.allocateVariables(rule)
    assertResult(Set(a, b, c, d1plusd2, d1, d2)) {
      var2reg.keySet
    }
    assert(var2reg.values.toSet.size == 6)
  }

  test("planner works with rule elements") {
    /**
      * Runs the planner on the rule
      *   Dist(a, c, d1+d2) :- Dist(a, b, d1), Dist(b, c, d2).
      * given facts
      *   Dist("x", "y", Dst(5)), Dist("y", "z", Dst(6))
      * and expects the following new fact to be inferred:
      *   Dist("x", "z", Dst(11))
      */
    val lattice = DistLattice
    val ShortestDist = new SimpleLatMap(lattice, 2)
    val ShortestDistPrime = new SimpleLatMap(lattice, 2)
    val myTranslator = new Translator()
    implicit def to_i(x: String): Int = myTranslator.toInt(x)

    ShortestDist.put(Array("x", "y"), lattice.Dst(5))
    ShortestDist.put(Array("y", "z"), lattice.Dst(6))
    ShortestDist.flushWrites()

    val a = KeyVariable("a")
    val b = KeyVariable("b")
    val c = KeyVariable("c")
    val d1 = LatVariable("d1")
    val d2 = LatVariable("d2")
    val d1plusd2 = LatVariable("d1+d2")

    val rule = Rule(
      new LatmapRuleElement(ShortestDistPrime, Seq(a, c, d1plusd2)),
      List(
        new LatmapRuleElement(ShortestDist, Seq(a, b, d1)),
        new LatmapRuleElement(ShortestDist, Seq(b, c, d2)),
        // TODO: Remove need for casting for TransferFnRuleElements.
        new TransferFnRuleElement((a: Array[Any]) => {
          (a(0), a(1)) match {
            case (DistLattice.NegInfinity, _) => DistLattice.NegInfinity
            case (_, DistLattice.NegInfinity) => DistLattice.NegInfinity
            case (DistLattice.Dst(n1), DistLattice.Dst(n2)) => DistLattice.Dst(n1 + n2)
            case _ => DistLattice.Infinity
          }
        }, Seq(d1, d2), d1plusd2)
      )
    )
    // Create a plan starting with the ShortestDist(a, b, d1) body element
    val planner = new Planner()
    val plan = planner.plan(rule, 0)
    val evalContext = new EvalContext {
      override val keyRegs: Array[Int] = new Array[Int](rule.numKeyVars)
      override val latRegs: Array[Any] = new Array[Any](rule.numLatVars)
      override val translator: Translator = myTranslator
    }

    plan.planElements.go(evalContext)
    ShortestDistPrime.flushWrites()
    assert(ShortestDistPrime.keyIterator.size == 1)
    assert(ShortestDistPrime.get(Array("x", "z")) == Dst(11))
  }
}
