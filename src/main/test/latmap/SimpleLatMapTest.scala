package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers

class SimpleLatMapTest extends FunSuite with Matchers {
    test("SimpleLatMap basic functionality") {
        val latmap = new SimpleLatMap(DistLattice, 5)
        val lattice = latmap.lattice
        def Dst = DistLattice.Dst

        latmap.get(Array(1, 2, 3, 4, 5)) shouldEqual DistLattice.Infinity

        latmap.put(Array(1, 2, 3, 4, 5), Dst(7)) shouldEqual Some(Dst(7))
        latmap.put(Array(1, 2, 3, 4, 5), Dst(8)) shouldEqual None
        latmap.put(Array(1, 2, 3, 4, 5), Dst(6)) shouldEqual Some(Dst(6))
        latmap.put(Array(1, 2, 3, 4, 6), DistLattice.Infinity) shouldEqual None
        latmap.put(Array(6, 2, 3, 4, 5), DistLattice.NegInfinity) shouldEqual Some(DistLattice.NegInfinity)

        latmap.get(Array(1, 2, 3, 4, 5)) shouldEqual Dst(6)
        latmap.get(Array(1, 2, 3, 4, 6)) shouldEqual DistLattice.Infinity
        latmap.get(Array(6, 2, 3, 4, 5)) shouldEqual DistLattice.NegInfinity

        latmap.put(Array(1, 2, 3, 4, 5), Dst(-2)) shouldEqual Some(Dst(-2))
        latmap.get(Array(1, 2, 3, 4, 5)) shouldEqual Dst(-2)

        TestUtils.testKeysEqual(
            latmap.keyIterator,
            List(
                Array(1, 2, 3, 4, 5),
                Array(1, 2, 3, 4, 6),
                Array(6, 2, 3, 4, 5)))
    }
}