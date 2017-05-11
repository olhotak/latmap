package latmap

import org.scalatest.FunSuite

class SolverTest extends FunSuite {
  test("solver") {
    val solver = new Solver()
    /*
    val sumDist = TransferFn(
      (x: DistLattice.Elem, y: DistLattice.Elem) => (x, y) match {
        case (DistLattice.NegInfinity, _) => DistLattice.NegInfinity
        case (_, DistLattice.NegInfinity) => DistLattice.NegInfinity
        case (DistLattice.Dst(d1), DistLattice.Dst(d2)) => DistLattice.Dst(d1 + d2)
        case _ => DistLattice.Infinity
      },
      List(LatVar("d1"), LatVar("d2"))
    )
    solver.solve(List(
      FlixRule(FlixAtom("Dist", List(KeyVar("x"), KeyVar("z"), sumDist)), List(
        FlixAtom("Dist", List(KeyVar("x"), KeyVar("y"), LatVar("d1"))),
        FlixAtom("Dist", List(KeyVar("y"), KeyVar("z"), LatVar("d2")))
      )
    )))
    */
    // potential DSL:
//    solver.solve(List(
//      Reachable(x, z) :- Reachable(x, y), Reachable(y, z).
//      Reachable("a", "b").
//      Reachable("b", "c").
//    ))
//    solve.solve(
//      Dist(x, z, sumDist(d1, d2)) :-
//        Dist(x, y, d1),
//        Dist(y, z, d2).
//    ))
  }
}
