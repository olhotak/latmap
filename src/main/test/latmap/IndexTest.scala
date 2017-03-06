package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.util.Random

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
                i1.getCollection(Array(1, 1, 1, 1, 1)),
                latmap.keyIterator)
        }
            
        test(s"${name}: i2") {
            TestUtils.testKeysEqual(
                Array(0, 0, 0, 0, 0) +: i2.getCollection(Array(1, 1, 1, 1, 1)),
                latmap.keyIterator)
        }
            
        test(s"${name}: i3") {
            TestUtils.testKeysEqual(
                Array(0, 0, 0, 0, 0) +:
                Array(1, 0, 0, 0, 0) +:
                Array(1, 1, 0, 0, 0) +:
                i3.getCollection(Array(1, 1, 1, 1, 1)),
                latmap.keyIterator)
        }
    }
    
    def stressTest(
            name: String,
            f1: (LatMap[_], Set[Int]) => Index,
            f2: (LatMap[_], Set[Int]) => Index,
            writes: Int,
            reads: Int,
            writers1: Int,
            readers1: Int,
            writers2: Int,
            readers2: Int): Unit = {
        require(writers1 == 1) // TODO
        require(readers1 == 1) // TODO
        require(writers2 == 1) // TODO
        require(readers2 == 1) // TODO
        
        test(s"Stress test with $name on $writes writes and then $reads reads; ($writers1/$readers1) ($writers2/$readers2)") {
            var nextSecond = 0
            val rng = new Random(123)
            def randomKey(): Array[Int] = {
                nextSecond += 1
                Array(rng.nextInt(10), rng.nextInt(2), nextSecond, nextSecond, nextSecond)
            }
            val latmap = new SimpleLatMap(DistLattice)
            val lattice = latmap.lattice
            
            val i1 = f1(latmap, Set(0, 1))
            val i2 = f2(latmap, Set(0, 1))
            
            (1 to 10) foreach { _ =>
                (1 to writes) foreach { _ =>
                    latmap.put(randomKey(), lattice.top)
                }
                
                (1 to reads) foreach { _ =>
                    val k = randomKey()
                    TestUtils.testKeysEqual(i1.getCollection(k), i2.getCollection(k))
                }
            }
        }
    }
    
    testIndex("NaiveIndex", new NaiveIndex(_, _))
    testIndex("HashMapIndex", new HashMapIndex(_, _))
    stressTest(
            "NaiveIndex/HashMapIndex",
            new NaiveIndex(_, _),
            new HashMapIndex(_, _),
            1000,
            1000,
            1,
            1,
            1,
            1)
}