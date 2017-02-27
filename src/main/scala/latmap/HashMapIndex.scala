package latmap

import scala.collection.mutable.HashMap
import java.util.Arrays

class HashMapIndex(
        val latticeMap: LatMap[_],
        val positions: Set[Int]) extends Index {
  
  private val posns: Array[Int] = positions.toArray
  private val notposns: Array[Int] = {
    for {
      i <- 0 until latticeMap.arity
      if !positions.contains(i)
    } yield i
  }.toArray
  
  private val empty = -1
  private val entryLen = latticeMap.arity
  private var size = 0
  private var capacity = 0 // always a power of 2!
  private var store: Array[Int] = Array()
  
  override def get(keys: Array[Int]): Iterator[Array[Int]] = {
    for {
      entry <- latticeMap.keyIterator
      if positions.forall(x => keys(x) == entry(x))
    } yield entry
  }
  
  // Same result as java.util.Arrays.hashCode
  private def hash(keys: Array[Int]): Int = {
    var i = 0
    var result = 1
    while (i < posns.length) {
      val x = keys(posns(i))
      result = result * 31 + x
      i += 1
    }
    result
  }
  
  private def resize(len: Int): Unit = {
    val newStore = new Array[Int](len)
    Arrays.fill(newStore, empty)
    {
      var i = 0
      while (i < store.length) {
        if 
        i += entryLen
      }
    }
  }
  
  private def insert(keys: Array[Int]): Unit = {
    val h = hash(keys) 
  }
    
  override def put(keys: Array[Int]): Boolean = {
    size += 1
    if (size * 2 > capacity)
      resize(capacity * 2)
    insert(keys)
    false
  }
  
  resize(16)
}
