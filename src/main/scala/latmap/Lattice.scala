package latmap

trait Lattice {
  type Elem
  def leq(other: Elem): Elem
  def lub(other: Elem): Elem
  def bottom: Elem
}
