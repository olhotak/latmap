package latmap

trait Lattice {
  type Elem
  def leq(a: Elem, b: Elem): Boolean
  def lub(a: Elem, b: Elem): Elem
  def glb(a: Elem, b: Elem): Elem
  def bottom: Elem
  def top: Elem
}
