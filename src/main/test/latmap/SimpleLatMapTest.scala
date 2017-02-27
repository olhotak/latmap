package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers

class SimpleLatMapTest extends FunSuite with Matchers {
    test("SimpleLatMap basic functionality") {
        val latmap = new SimpleLatMap(DistLattice)
        val lattice = latmap.lattice

        latmap.get(Array(1, 2, 3, 4, 5)) shouldBe lattice.bottom

        latmap.put(Array(1, 2, 3, 4, 5), DistLattice.Dst(7))
        latmap.put(Array(1, 2, 3, 4, 6), DistLattice.Infinity)
        latmap.put(Array(6, 2, 3, 4, 5), DistLattice.NegInfinity)

        latmap.get(Array(1, 2, 3, 4, 5)) shouldEqual DistLattice.Dst(7)
        latmap.get(Array(1, 2, 3, 4, 6)) shouldEqual DistLattice.Infinity
        latmap.get(Array(6, 2, 3, 4, 5)) shouldEqual DistLattice.NegInfinity
        
        TestUtils.testKeysEqual(
            latmap.keyIterator,
            List(
                Array(1, 2, 3, 4, 5),
                Array(1, 2, 3, 4, 6),
                Array(6, 2, 3, 4, 5)))
    }
}