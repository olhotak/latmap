package latmap

import scala.collection.mutable

/** A (mutable) map from tuples of keys to lattice elements. */
trait LatMap[T <: Lattice] {
  val lattice: Lattice

  /** The number of keys (columns). */
  def arity: Int

  /** Looks up the lattice element associated with a key.
    * The default value is bottom (if the keys are not in the map).
    * Precondition: keys.size == arity
    */
  def get(keys: Array[Int]): lattice.Elem

  /** Iterator over all the keys mapped to a non-bottom lattice element. */
  def keyIterator: Iterator[Array[Int]]

  /** Update the lattice element for a particular tuple of keys.
    * If the tuple is not yet in the map, associates it with the given lattice element.
    * Otherwise, associates the keys tuple with old.lub(elem), where old is the
    * lattice element previously associated with the keys tuple.
    * Also calls put on all of the indexes.
    * Precondition: keys.size == arity
    * Returns the new lattice element associated with keys.
    */
  def put(keys: Array[Int], elem: lattice.Elem): Option[lattice.Elem]

  /** A list of indexes that have been associated with this lattice map. */
  def indexes: mutable.ListBuffer[Index]

  def addIndex(index: Index): Unit
}
