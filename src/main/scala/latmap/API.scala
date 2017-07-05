package latmap

trait API {
  type Constant <: Term
  type Variable <: Term with APIVariable
  type BodyElem <: APIBodyElem
  type Atom <: BodyElem with APIAtom
  type Const <: BodyElem
  type Relation <: APIRelation

  trait Term
  trait APIVariable extends Term {
    def :=(constant: Any): Const
  }
  def variable(): Variable

  trait APIRelation {
    def apply(terms: Term*): Atom
  }
  def relation(arity: Int, lattice: Lattice): Relation
  def relation(arity: Int): Relation

  trait APIBodyElem
  trait APIAtom extends APIBodyElem {
    def :-(body: BodyElem*): Unit
  }

  implicit def anyConst(a: Any): Constant

  def solve(): Unit
}

object API {
  def apply(): API = new APIImpl
}

