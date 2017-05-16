package latmap

trait API {
  trait APIBodyElem
  type BodyElem <: APIBodyElem

  trait APIVariable {
    def :=(constant: Any): Const
  }
  type Variable <: APIVariable
  def variable(): Variable

  trait APIRelation {
    def apply(vars: Variable*): Atom
  }
  type Relation <: APIRelation
  def relation(arity: Int, lattice: Lattice): Relation
  def relation(arity: Int): Relation

  trait APIAtom extends APIBodyElem {
    def :-(body: BodyElem*): Unit
  }
  type Atom <: APIAtom with BodyElem

  type Const <: BodyElem

  type Rule

  def solve(): Unit
}

object API {
  def apply(): API = new APIImpl
}

