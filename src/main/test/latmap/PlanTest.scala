package latmap

import org.scalatest.FunSuite

class PlanTest extends FunSuite {
  test("IndexScan basic functionality") {
    val lattice = DistLattice
    val latmap = new SimpleLatMap(lattice, 2)
    val index: Index = new NaiveIndex(latmap, Set(1))

    latmap.addIndex(index)
    latmap.put(Array(1, 2), lattice.Dst(5))
    latmap.put(Array(2, 2), lattice.Dst(6))
    latmap.put(Array(2, 2), lattice.Dst(7))
    latmap.put(Array(2, 2), lattice.Dst(8))

    val planElement = IndexScan(index, mergeLat = false, Array(0, 1), Array(), 0,
      WriteToLatMap())

    val evalContext = new EvalContext {
      override val latRegs: Array[Any] = new Array(10)
      override val translator: Translator = new Translator()
      override val keyRegs: Array[Int] = Array(0, 2, 0, 0, 0, 0, 0, 0, 0, 0)
    }

    planElement.go(evalContext)
  }
}
