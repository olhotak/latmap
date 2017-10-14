package latmap

object DistLattice extends Lattice {
  /**
    * The min distance lattice, e.g. used in Floyd Warshall.
    *   NegInfinity
    *    |
    * Int.MinValue
    *    |
    *   ...
    *    |
    *    -1
    *    |
    *    0
    *    |
    *    1
    *    |
    *   ...
    *    |
    *  Int.MaxValue
    *    |
    *   Infinity
    */
  sealed trait Dist
  case object NegInfinity extends Dist
  case object Infinity extends Dist
  case class Dst(d: Int) extends Dist

  type Elem = Dist
  def leq(a: Dist, b: Dist): Boolean = (a, b) match {
    case (_, NegInfinity) => true
    case (Infinity, _) => true
    case (Dst(d1), Dst(d2)) => d1 >= d2
    case _ => false
  }
  def lub(a: Dist, b: Dist): Dist = (a, b) match {
    case (x, Infinity) => x
    case (Infinity, x) => x
    case (Dst(d1), Dst(d2)) => Dst(math.min(d1, d2))
    case _ => NegInfinity
  }
  def glb(a: Dist, b: Dist): Dist = (a, b) match {
    case (NegInfinity, x) => x
    case (x, NegInfinity) => x
    case (Dst(d1), Dst(d2)) => Dst(math.max(d1, d2))
    case _ => Infinity
  }
  def bottom: Dist = Infinity
  def top: Dist = NegInfinity
}

object TwoPointLattice extends Lattice {
  /**
    * The two-point lattice.
    * top
    *  |
    * bottom
    */
  sealed trait TwoPointLatticeElem
  case object Top extends TwoPointLatticeElem
  case object Bot extends TwoPointLatticeElem
  type Elem = TwoPointLatticeElem

  def leq(a: Elem, b: Elem): Boolean = (a, b) match {
    case (Bot, _) => true
    case (_, Top) => true
    case _ => false
  }

  def lub(a: Elem, b: Elem): Elem = (a, b) match {
    case (Top, _) => Top
    case (_, Top) => Top
    case _ => Bot
  }

  def glb(a: Elem, b: Elem): Elem = (a, b) match {
    case (Bot, _) => Bot
    case (_, Bot) => Bot
    case _ => Top
  }

  def bottom: Elem = Bot
  def top: Elem = Top
}

object SULattice extends Lattice {
  sealed trait SUElem
  case object Top extends SUElem
  case class Single(str: String) extends SUElem
  case object Bottom extends SUElem

  type Elem = SUElem
  def bottom: SUElem = Bottom
  def top: SUElem = Top

  def equ(e1: SUElem, e2: SUElem): Boolean = e1 == e2

  def leq(e1: SUElem, e2: SUElem): Boolean = (e1,e2) match {
    case (Bottom, _) => true
    case (_, Top) => true
    case (Single(s1), Single(s2)) => s1 == s2
    case _ => false
  }
  def lub(e1: SUElem, e2: SUElem): SUElem = (e1,e2) match {
    case (Bottom, _) => e2
    case (_, Bottom) => e1
    case (Single(s1), Single(s2)) => if (s1 == s2) Single(s1) else Top
    case _ => Top
  }
  def glb(e1: SUElem, e2: SUElem): SUElem = (e1,e2) match {
    case (Top, _) => e2
    case (_, Top) => e1
    case (Single(s1), Single(s2)) => if (s1 == s2) Single(s1) else Bottom
    case _ => Bottom
  }
}


