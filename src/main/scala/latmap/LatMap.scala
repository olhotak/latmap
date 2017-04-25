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

  /** Prepares to add the (keys, elem) pair in the next write phase.
    * Precondition: keys.size == arity
    * Returns the least upper bound of the current value and the given value.
    */
  def put(keys: Array[Int], elem: lattice.Elem): Option[lattice.Elem]
  
  /** Client must call writePhase1, then betweenWritePhases, then writePhase2.
    * writePhase1 and writePhase2 are idempotent and can be called concurrently.
    * betweenWritePhases must be called exactly once between calls to writePhase1
    * and writePhase2.
    * So, it cannot be called concurrently.
    * 
    * If A is a call to writePhase1 and C is a call to writePhase2, one of the
    * following must be true:
    * (1) C happens-before A, or
    * (2) There is a call to betweenWritePhases B such that A happens-before B
    * and B happens-before C.
    * 
    * In other words, you should join on all the threads calling writePhase1,
    * then call betweenWritePhases, then join on all the threads calling
    * writePhase2 before calling writePhase1 again.
    */
  def writePhase1(): Unit
  def betweenWritePhases(): Unit
  def writePhase2(): Unit
  
  /**
   * Simple single-threaded write flushing
   */
  def flushWrites() = {
      writePhase1()
      betweenWritePhases()
      writePhase2()
  }

  /** A list of indexes that have been associated with this lattice map. */
  def indexes: mutable.ListBuffer[Index]

  /** Select the index to use for the given set of bound variables.
    * If no such index exists, create a new NaiveIndex and use it. */
  def selectIndex(boundVars: Set[Int]): Index = {
    var best: Index = null
    for (index <- indexes) {
      if (index.positions.subsetOf(boundVars) && (best == null || best.positions.size > best.positions.size)) {
        best = index
      }
    }
    if (best == null) {
      best = new NaiveIndex(this, boundVars)
      indexes += best
    }
    best
  }

  def addIndex(index: Index): Unit
}
