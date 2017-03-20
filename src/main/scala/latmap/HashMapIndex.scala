package latmap

import scala.collection.mutable.HashMap
import java.util.Arrays

class HashMapIndex(
        val latticeMap: LatMap[_],
        val positions: Set[Int]) extends Index {
  
  private val posns: Array[Int] = positions.toArray
  private val notposns: Array[Int] = ((0 until latticeMap.arity).toSet -- positions).toArray
  
  private val empty = -1
  private val entryLen = latticeMap.arity
  private var size = 0
  private var capacity = 0 // always a power of 2!
  private var store: Array[Int] = Array()
  private var jump: Array[Int] = Array()
  
  private def mask = capacity - 1 // since capacity is a power of 2
  
  private class EntryIterator(
          val keys: Array[Int],
          var pos: Int) extends Iterator[Array[Int]] {
      
      def isMatch: Boolean = {
          var i = 0
          while (i < posns.length) {
              if (keys(posns(i)) != store(pos * entryLen + i))
                  return false
              i += 1
          }
          return true
      }
      
      def isEmptyEntry = store(pos * entryLen) == empty
      
      def validatePosn() = {
          do {
              pos &= mask
          } while (
              if (isEmptyEntry || isMatch)
                  false
              else {
                  pos += 1
                  true
              }
          )
      }
      validatePosn()
      
      val resultHolder = new Array[Int](entryLen)
      
      def hasNext = !isEmptyEntry
      def next() = {
          {
              var i = 0
              while (i < posns.length) {
                  resultHolder(posns(i)) = store(pos * entryLen + i)
                  i += 1
              }
          }
          {
              var i = 0
              while (i < notposns.length) {
                  resultHolder(notposns(i)) = store(pos * entryLen + i + posns.length)
                  i += 1
              }
          }
          pos += 1
          validatePosn()
          resultHolder
      }
  }
  
  override def get(keys: Array[Int]): Iterator[Array[Int]] = {
      new EntryIterator(keys, hash(keys))
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
  
  // Similar to hash, but uses the values in `store`
  private def hashAt(arr: Array[Int], pos: Int): Int = {
    var i = 0
    var result = 1
    while (i < posns.length) {
      val x = arr(pos + i)
      result = result * 31 + x
      i += 1
    }
    result
  }
  
  private def resize(len: Int): Unit = {
    assert((len & (len - 1)) == 0) // len must be a power of 2
    val newMask = len - 1
    val newStore = new Array[Int](len * entryLen)
    jump = new Array[Int](len)
    Arrays.fill(newStore, empty)
    
    {
      var i = 0
      while (i < store.length) {
        if (store(i) != empty) {
          val startIndex = hashAt(store, i) & newMask
          var idx = (startIndex + jump(startIndex)) & newMask
          while (newStore(entryLen * idx) != empty) {
            idx = (idx + 1) & newMask
          }
          jump(startIndex) = idx - startIndex + len
          var j = 0
          while (j < entryLen) {
            newStore(idx * entryLen + j) = store(i + j)
            j += 1
          }
        }
        i += entryLen
      }
    }
    capacity = len
    store = newStore
  }
  
  private def insert(keys: Array[Int]): Unit = {
    val startIndex = hash(keys) & mask
    var idx = (startIndex + jump(startIndex)) & mask
    while (store(idx * entryLen) != empty) {
      idx = (idx + 1) & mask
    }
    
    {
      var i = 0
      while (i < posns.length) {
        store(idx * entryLen + i) = keys(posns(i))
        i += 1
      }
    }
    {
      var i = 0
      while (i < notposns.length) {
        store(idx * entryLen + posns.length + i) = keys(notposns(i))
        i += 1
      }
    }
    jump(startIndex) = idx - startIndex + capacity
  }
    
  override def put(keys: Array[Int]): Unit = {
    size += 1
    if (size * 2 > capacity)
      resize(capacity * 2)
    insert(keys)
  }
  
  resize(16)
}
