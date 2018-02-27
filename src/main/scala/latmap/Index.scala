package latmap

/** An index for a subset of the key columns of a lattice map.
  * The goal is, given the values of a subset of the keys, find
  * all full tuples of keys such that latticeMap(keys) is not bottom.
  */
trait Index {
  val latMap: AbstractLatMap
  val positions: Array[Int]

  /** Given the values of a subset of the keys, find
    * all full tuples of keys such that latticeMap(keys) is not bottom.
    * Precondition: keys.size == latticeMap.arity
    * Note: The values keys[i] are relevant only for values of i in the positions set.
    * For other values of i, keys[i] is irrelevant and ignored.
    * Postcondition: forall p in returned collection: p.size == latticeMap.arity
    */
  def get(row: Array[Int]): Iterator[Array[Int]]
  
  /** Inform the index that a new tuple has been added to the latticeMap.
    * Precondition: keys.size == latticeMap.arity
    */
  def put(row: Array[Int]): Unit = {}
  
  def positionString: String = (0 until latMap.width).map{p => if(positions.contains(p)) 'K' else '.'}.mkString
}
