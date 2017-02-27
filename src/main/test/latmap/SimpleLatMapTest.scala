package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers

class SimpleLatMapTest extends FunSuite with Matchers {
    test("SimpleLatMap basic functionality") {
        val latmap = new SimpleLatMap
        val lattice = latmap.lattice
        
        latmap.get(Array(1, 2, 3, 4, 5)) shouldBe lattice.bottom
        
        latmap.put(Array(1, 2, 3, 4, 5), DistLattice.Dst(7))
        latmap.put(Array(1, 2, 3, 4, 6), DistLattice.Infinity)
        latmap.put(Array(6, 2, 3, 4, 5), DistLattice.NegInfinity)
        
        latmap.get(Array(1, 2, 3, 4, 5)) shouldEqual DistLattice.Dst(7)
        latmap.get(Array(1, 2, 3, 4, 6)) shouldEqual DistLattice.Infinity
        latmap.get(Array(6, 2, 3, 4, 5)) shouldEqual DistLattice.NegInfinity
        
        latmap.keyIterator.map(_.toList).toSet shouldEqual Set(
                List(1, 2, 3, 4, 5),
                List(1, 2, 3, 4, 6),
                List(6, 2, 3, 4, 5))
    }
}