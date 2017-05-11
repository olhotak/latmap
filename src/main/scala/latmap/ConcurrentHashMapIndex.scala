package latmap

import scala.collection.mutable.HashMap
import java.util.Arrays
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.locks.ReentrantReadWriteLock

class ConcurrentHashMapIndex(
        val latticeMap: LatMap[_ <: Lattice],
        val positions: Set[Int],
        startingCapacity: Int) extends Index {
  require(startingCapacity >= 128)
  
  private val posns: Array[Int] = positions.toArray
  private val notposns: Array[Int] = ((0 until latticeMap.arity).toSet -- positions).toArray
  
  private val empty = -1
  private val entryLen = posns.length + 1
  private val size = new AtomicInteger(0)
  private var capacity = 0 // always a power of 2!
  private var store: Array[Int] = Array()
  private var lists: Array[ConcurrentLinkedQueue[Array[Int]]] = Array()
  private val nextNode = new AtomicInteger(0)
  
  private val LOCK_COUNT = 64
  private val locks = Array.fill(LOCK_COUNT)(new Object)
  
  private val resizeLock = new ReentrantReadWriteLock
  
  private def mask = capacity - 1 // since capacity is a power of 2
  
  private class EntryIterator(
          val keys: Array[Int],
          var pos: Int) extends Iterator[Array[Int]] {
      var iter: java.util.Iterator[Array[Int]] = null
      
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
          if (!isEmptyEntry) {
              iter = lists(store(pos * entryLen + (entryLen - 1))).iterator()
          }
      }
      validatePosn()
      
      val resultHolder = new Array[Int](entryLen)
      
      def hasNext = !isEmptyEntry
      def next() = {
          val result = iter.next()
          if (!iter.hasNext()) {
              pos += 1
              validatePosn()
          }
          result
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
    val newStore = new Array[Int](len * entryLen)
    Arrays.fill(newStore, empty)
    val newLists = new Array[ConcurrentLinkedQueue[Array[Int]]](len)
    val currentSize = size.get()
    
    {
      var i = 0
      while (i < currentSize) {
        newLists(i) = lists(i)
        i += 1
      }
    }
    
    val newMask = len - 1
    var idx = 0
    while (idx < capacity) {
        if (store(idx * entryLen) != empty) {
            var newIdx = hashAt(store, idx * entryLen) & newMask
            while (newStore(newIdx * entryLen) != empty)
                newIdx = (newIdx + 1) & newMask
            var i = 0
            while (i < entryLen) {
                newStore(newIdx * entryLen + i) = store(idx * entryLen + i)
                i += 1
            }
        }
        idx += 1
    }
    capacity = len
    store = newStore
    lists = newLists
  }
  
  private def insert(keys: Array[Int]): Unit = {
    insertImpl(hash(keys) & mask)
    def insertImpl(idx: Int): Unit = {
        var done = false
        if (store(idx * entryLen) == empty) {
            locks(idx / LOCK_COUNT).synchronized {
                if (store(idx * entryLen) == empty) {
                    var i = 0
                    while (i < posns.length) {
                        store(idx * entryLen + i) = keys(posns(i))
                        i += 1
                    }
                    val listIdx = nextNode.getAndIncrement()
                    store(idx * entryLen + i) = listIdx
                    lists(listIdx) = new ConcurrentLinkedQueue[Array[Int]]
                    lists(listIdx).add(Arrays.copyOf(keys, keys.length))
                    size.incrementAndGet()
                    done = true
                }
            }
        }
        if (!done) {
            var ok = true
            var i = 0
            while (i < posns.length && ok) {
                if (store(idx * entryLen + i) != keys(posns(i)))
                    ok = false
                i += 1
            }
            val listIdx = store(idx * entryLen + (entryLen - 1))
            if (ok && listIdx != empty) {
                lists(listIdx).add(Arrays.copyOf(keys, keys.length))
            }
            else {
                insertImpl((idx + 1) & mask)
            }
        }
    }
  }
    
  override def put(keys: Array[Int]): Unit = {
    resizeLock.readLock().lock()
    insert(keys)
    resizeLock.readLock().unlock()
    
    if (size.get() * 2 > capacity) {
      resizeLock.writeLock().lock()
      if (size.get() * 2 > capacity) {
        resize(capacity * 2)
      }
      resizeLock.writeLock().unlock()
    }
  }
  
  // Smallest power of 2 greater than or equal to x
  def nextPowerOf2(x: Int): Int = {
      if ((x & (x - 1)) == 0)
          x
      else
          nextPowerOf2(x + (x & -x))
  }
  
  def prepareForWrites(number: Int): Unit = {
      val newCapacity = nextPowerOf2(size.get() + number)
      if (newCapacity > capacity)
      {
          resize(newCapacity)
      }
  }
  
  resize(startingCapacity)
}
