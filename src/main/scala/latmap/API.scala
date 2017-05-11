package latmap

trait API {
  trait APIVariable
  type Variable <: APIVariable
  def variable(): Variable

  trait APIRelation {
    def apply(vars: Variable*): Atom
  }
  type Relation <: APIRelation
  def relation(arity: Int, lattice: Lattice): Relation
  def relation(arity: Int): Relation

  trait APIAtom {
    def :-(atoms: Atom*): Unit
  }
  type Atom <: APIAtom
  type Rule

  def solve(): Unit
}

object API {
  def apply(): API = new APIImpl
}

