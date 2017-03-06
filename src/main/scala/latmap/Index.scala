package latmap

import scala.collection.BitSet
import scala.collection.mutable.ArrayBuffer
import java.util.Arrays

/** An index for a subset of the key columns of a lattice map.
  * The goal is, given the values of a subset of the keys, find
  * all full tuples of keys such that latticeMap(keys) is not bottom.
  */
trait Index {
  val latticeMap: LatMap[_]
  
  latticeMap.addIndex(this)

  /** The subset of the (0-based) positions of the keys indexed.
    * Invariant: forall i. 0 <= keys.get(i) < latticeMap.arity
    **/
  val positions: Set[Int]

  /** Given the values of a subset of the keys, find
    * all full tuples of keys such that latticeMap(keys) is not bottom.
    * Precondition: keys.size == latticeMap.arity
    * Note: The values keys[i] are relevant only for values of i in the positions set.
    * For other values of i, keys[i] is irrelevant and ignored.
    * Postcondition: forall p in returned collection: p.size == latticeMap.arity
    */
  def get(keys: Array[Int]): Iterator[Array[Int]]
  
  def getCollection(keys: Array[Int]): Array[Array[Int]] = {
      val buf = new ArrayBuffer[Array[Int]]
      val it = get(keys)
      while (it.hasNext) {
          val arr = it.next()
          buf += Arrays.copyOf(arr, arr.length)
      }
      buf.toArray
  }

  /** Inform the index that a new tuple has been added to the latticeMap.
    * Precondition: keys.size == latticeMap.arity
    */
  def put(keys: Array[Int]): Unit
}
