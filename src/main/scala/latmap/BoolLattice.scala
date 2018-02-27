package latmap

/** Boolean lattice.
  * For using LatMap with relations.
  */
object BoolLattice extends Lattice {
  // true
  //   |
  // false
  type Elem = Boolean
  def leq(a: Elem, b: Elem): Boolean = a || !b
  def lub(a: Elem, b: Elem): Elem = a || b
  def glb(a: Elem, b: Elem): Elem = a && b
  def bottom: Elem = false
  def top: Elem = true
  override def toString: String = "B"
}
