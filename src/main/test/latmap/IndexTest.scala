package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await

class IndexTest extends FunSuite with Matchers {
    def testIndex(name: String, f: (LatMap[_ <: Lattice], Set[Int]) => Index) = {
        val latmap = new SimpleLatMap(DistLattice, 5)
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
        latmap.flushWrites()
        
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
            f1: (LatMap[DistLattice.type], Set[Int]) => Index,
            f2: (LatMap[DistLattice.type], Set[Int]) => Index,
            writes: Int,
            reads: Int,
            writers1: Int,
            readers1: Int,
            writers2: Int,
            readers2: Int,
            period: Int = 20): Unit = {
        require(writers1 == writers2) // TODO
        require(readers1 == readers2) // TODO
        
        test(s"Stress test with $name on $writes writes and $reads reads; ($writers1/$readers1) ($writers2/$readers2)") {
            val nextSecond = new AtomicInteger(0)
            def randomKey(): Array[Int] = {
                val v = nextSecond.incrementAndGet()
                Array(v % period / 2, v % period % 2, v, v, v)
            }
            val latmap = new SimpleLatMap(DistLattice, 5)
            val lattice = latmap.lattice
            
            val i1 = f1(latmap, Set(0, 1))
            val i2 = f2(latmap, Set(0, 1))
            var writeTot: Long = 0
            var readTot: Long = 0
            
            (1 to 10) foreach { _ =>
                val writers = for (i <- 1 to writers1) yield Future.apply {
                    (1 to (writes / 10 / writers1)) foreach { _ =>
                        latmap.put(randomKey(), lattice.top) 
                    }
                }
                val writing = Future.sequence(writers)
                val writeStart = System.currentTimeMillis()
                Await.result(writing, Duration.Inf)
                writeTot += System.currentTimeMillis() - writeStart
                
                val futures = for (i <- 1 to readers1) yield Future.apply {
                    (1 to (reads / 10 / readers1)) forall { _ =>
                        val k = randomKey()
                        TestUtils.areKeysEqual(i1.getCollection(k), i2.getCollection(k))
                    }
                }
                val computation = Future.sequence(futures)
                val readStart = System.currentTimeMillis()
                val result = Await.result(computation, Duration.Inf)
                readTot += System.currentTimeMillis() - readStart
                assert(result.forall(identity))
            }
            println(s"$name took approximately $writeTot ms to write and $readTot ms to read")
        }
    }
    
    testIndex("NaiveIndex", new NaiveIndex(_, _))
    testIndex("HashMapIndex", new HashMapIndex(_, _))
    stressTest(
            "NoOpIndex",
            new NoOpIndex(_, _),
            new NoOpIndex(_, _),
            200000,
            100,
            10,
            10,
            10,
            10)
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
    stressTest(
            "NaiveIndex/HashMapIndex [concurrent reads]",
            new NaiveIndex(_, _),
            new HashMapIndex(_, _),
            1000,
            1000,
            1,
            10,
            1,
            10)
            // TODO this test fails
//    stressTest(
//            "NaiveIndex/HashMapIndex [concurrent writes]",
//            new NaiveIndex(_, _),
//            new HashMapIndex(_, _),
//            1000,
//            1000,
//            10,
//            1,
//            10,
//            1)
    stressTest(
            "HashMapIndex/HashMapIndex [perf test]",
            new HashMapIndex(_, _),
            new HashMapIndex(_, _),
            200000,
            100,
            1,
            10,
            1,
            10)
    stressTest(
            "ConcurrentHashMapIndex/ConcurrentHashMapIndex [perf test]",
            new ConcurrentHashMapIndex(_, _, 1024),
            new ConcurrentHashMapIndex(_, _, 1024),
            200000,
            100,
            10,
            10,
            10,
            10)
    stressTest(
            "ConcurrentHashMapIndex/ConcurrentHashMapIndex [perf test]",
            new ConcurrentHashMapIndex(_, _, 1024),
            new ConcurrentHashMapIndex(_, _, 1024),
            200000,
            100,
            1,
            10,
            1,
            10)
    stressTest(
            "NoOpIndex p2",
            new NoOpIndex(_, _),
            new NoOpIndex(_, _),
            200000,
            100,
            10,
            10,
            10,
            10)
    stressTest(
            "ConcurrentHashMapIndex/ConcurrentHashMapIndex [perf test] 2",
            new ConcurrentHashMapIndex(_, _, 1024),
            new ConcurrentHashMapIndex(_, _, 1024),
            200000,
            100,
            10,
            10,
            10,
            10)
    stressTest(
            "ConcurrentHashMapIndex/ConcurrentHashMapIndex [perf test] 2",
            new ConcurrentHashMapIndex(_, _, 1024),
            new ConcurrentHashMapIndex(_, _, 1024),
            200000,
            100,
            1,
            10,
            1,
            10)
    stressTest(
            "NoOpIndex p3",
            new NoOpIndex(_, _),
            new NoOpIndex(_, _),
            200000,
            100,
            10,
            10,
            10,
            10)
    stressTest(
            "ConcurrentHashMapIndex/ConcurrentHashMapIndex with many distinct keys",
            new ConcurrentHashMapIndex(_, _, 128),
            new ConcurrentHashMapIndex(_, _, 128),
            200000,
            100,
            10,
            10,
            10,
            10,
            1000)
}