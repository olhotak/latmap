package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers

class IndexTest extends FunSuite with Matchers {
    def testIndex(name: String, f: (LatMap[_], Set[Int]) => Index) = {
        val latmap = new SimpleLatMap(DistLattice)
        val lattice = latmap.lattice
        
        val i1 = f(latmap, Set())
        val i2 = f(latmap, Set(0))
        val i3 = f(latmap, Set(0, 1, 2))
        
        latmap.put(Array(0, 0, 0, 0, 0), lattice.top)
        latmap.put(Array(1, 0, 0, 0, 0), lattice.top)
        latmap.put(Array(1, 1, 0, 0, 0), lattice.top)
        latmap.put(Array(1, 1, 1, 0, 0), lattice.top)
        latmap.put(Array(1, 1, 1, 1, 0), lattice.top)
        latmap.put(Array(1, 1, 1, 1, 1), lattice.top)
        
        test(s"${name}: i1") {
            TestUtils.testKeysEqual(
                i1.get(Array(1, 1, 1, 1, 1)),
                latmap.keyIterator)
        }
            
        test(s"${name}: i2") {
            TestUtils.testKeysEqual(
                Array(0, 0, 0, 0, 0) +: i2.get(Array(1, 1, 1, 1, 1)).toList,
                latmap.keyIterator)
        }
            
        test(s"${name}: i3") {
            TestUtils.testKeysEqual(
                Array(0, 0, 0, 0, 0) +:
                Array(1, 0, 0, 0, 0) +:
                Array(1, 1, 0, 0, 0) +:
                i3.get(Array(1, 1, 1, 1, 1)).toList,
                latmap.keyIterator)
        }
    }
    
    testIndex("NaiveIndex", new NaiveIndex(_, _))
}