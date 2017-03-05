package latmap

import scala.collection.BitSet

/** An index for a subset of the key columns of a lattice map.
  * The goal is, given the values of a subset of the keys, find
  * all full tuples of keys such that latticeMap(keys) is not bottom.
  */
trait Index {
  val latticeMap: LatMap[_]

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

  /** Inform the index that a new tuple has been added to the latticeMap.
    * Precondition: keys.size == latticeMap.arity
    */
  // TODO what does return value mean? --> should be Unit
  def put(keys: Array[Int]): Boolean
}
