package latmap

import com.sun.xml.internal.bind.v2.schemagen.xmlschema.TopLevelAttribute

/** Boolean lattice.
  * For using LatMap with relations.
  */
object ParityLattice extends Lattice {
  //      Top
  //    /     \
  //  Odd     Even
  //    \     /
  //      Bot
  sealed trait Parity
  case object Top extends Parity
  case object Odd extends Parity
  case object Even extends Parity
  case object Bot extends Parity

  type Elem = Parity
  def leq(a: Elem, b: Elem): Boolean = (a, b) match {
    case (Bot, _) => true
    case (Odd, Odd) => true
    case (Even, Even) => true
    case (_, Top) => true
    case _ => false
  }
  def lub(a: Elem, b: Elem): Elem = (a, b) match {
    case (Bot, _) => b
    case (_, Bot) => a
    case (Odd, Odd) => Odd
    case (Even, Even) => Even
    case _ => Top
  }
  def glb(a: Elem, b: Elem): Elem = (a, b) match {
    case (Top, _) => b
    case (_, Top) => a
    case (Odd, Odd) => Odd
    case (Even, Even) => Even
    case _ => Bot
  }
  def bottom: Elem = Bot
  def top: Elem = Top
}
