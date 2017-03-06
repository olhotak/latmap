package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers

// TODO: Install the bytecode viewer
// visualvm
// TODO: special case two-element lattice latmap

class SimpleLatMapTest extends FunSuite with Matchers {
    test("SimpleLatMap basic functionality") {
        val latmap = new SimpleLatMap(DistLattice)
        val lattice = latmap.lattice
        def Dst = DistLattice.Dst

        latmap.get(Array(1, 2, 3, 4, 5)) shouldBe lattice.bottom

        latmap.put(Array(1, 2, 3, 4, 5), Dst(7)) shouldEqual Dst(7)
        latmap.put(Array(1, 2, 3, 4, 6), DistLattice.Infinity) shouldEqual DistLattice.Infinity
        latmap.put(Array(6, 2, 3, 4, 5), DistLattice.NegInfinity) shouldEqual DistLattice.NegInfinity

        latmap.get(Array(1, 2, 3, 4, 5)) shouldEqual Dst(7)
        latmap.get(Array(1, 2, 3, 4, 6)) shouldEqual DistLattice.Infinity
        latmap.get(Array(6, 2, 3, 4, 5)) shouldEqual DistLattice.NegInfinity

        latmap.put(Array(1, 2, 3, 4, 5), Dst(-2)) shouldEqual Dst(-2)
        latmap.get(Array(1, 2, 3, 4, 5)) shouldEqual Dst(-2)

        TestUtils.testKeysEqual(
            latmap.keyIterator,
            List(
                Array(1, 2, 3, 4, 5),
                Array(1, 2, 3, 4, 6),
                Array(6, 2, 3, 4, 5)))
    }
}