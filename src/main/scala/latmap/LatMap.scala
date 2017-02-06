package latmap

/** A (mutable) map from tuples of keys to lattice elements. */
trait LatMap {
  val lattice: Lattice

  /** The number of keys (columns). */
  def arity: Int

  /** Looks up the lattice element associated with a key.
    * The default value is bottom (if the keys are not in the map).
    */
  def get(keys: Array[Any]): lattice.Elem

  /** Iterator over all the keys mapped to a non-bottom lattice element. */
  def keyIterator: Iterator[Array[Any]]

  /** Update the lattice element for a particular tuple of keys.
    * If the tuple is not yet in the map, associates it with the given lattice element.
    * Otherwise, associates the keys tuple with old.lub(elem), where old is the
    * lattice element previously associated with the keys tuple.
    * Also calls put on all of the indexes.
    * Precondition: keys.size == arity
    * Returns true iff the lattice element associated with the keys tuple changed.
    */
  def put(keys: Array[Any], elem: lattice.Elem): Boolean

  /** A list of indexes that have been associated with this lattice map. */
  def indexes: List[Index]

  def addIndex(index: Index): Unit
}
