package latmap

trait Lattice {
  type Elem
  def leq(a: Elem, b: Elem): Elem
  def lub(a: Elem, b: Elem): Elem
  def bottom: Elem
}
