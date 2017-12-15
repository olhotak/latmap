package latmap

trait API {
  type Constant <: Term
  type Variable <: Term with APIVariable
  type KeyVariable <: Variable
  type LatVariable <: Variable
  type BodyElem <: APIBodyElem
  type Atom <: BodyElem with APIAtom
  type Const <: BodyElem
  type Relation <: APIRelation

  trait Term
  trait APIVariable extends Term {
    def :=(constant: Any): Const
  }
  def variable(): KeyVariable
  def latVariable(lattice: Lattice): LatVariable

  trait APIRelation {
    def apply(terms: Term*): Atom
    def numFacts(): Int
    def dump(): Unit
  }
  def relation(arity: Int, lattice: Lattice, name: String): Relation
  def relation(arity: Int, lattice: Lattice): Relation = relation(arity, lattice, "")
  def relation(arity: Int, name: String): Relation = relation(arity, BoolLattice, name)
  def relation(arity: Int): Relation = relation(arity, "")

  trait APIBodyElem
  trait APIAtom extends APIBodyElem {
    def :-(body: BodyElem*): Unit
    def addIndex(terms: Term*): Unit
  }

  implicit def anyConst(a: Any): Constant

  def F(f: Function0[Boolean]): BodyElem
  def F[T1](f: Function1[T1, Boolean], t1: Term): BodyElem
  def F[T1,T2](f: Function2[T1, T2, Boolean], t1: Term, t2: Term): BodyElem
  def F[T1,T2,T3](f: Function3[T1, T2, T3, Boolean], t1: Term, t2: Term, t3: Term): BodyElem
  def F[T1,T2,T3,T4](f: Function4[T1, T2, T3, T4, Boolean], t1: Term, t2: Term, t3: Term, t4: Term): BodyElem
  def F[T1,T2,T3,T4,T5](f: Function5[T1, T2, T3, T4, T5, Boolean], t1: Term, t2: Term, t3: Term, t4: Term, t5: Term): BodyElem

  def T[R](r: Variable, f: Function0[R]): BodyElem
  def T[T1,R](r: Variable, f: Function1[T1, R], t1: Term): BodyElem
  def T[T1,T2,R](r: Variable, f: Function2[T1, T2, R], t1: Term, t2: Term): BodyElem
  def T[T1,T2,T3,R](r: Variable, f: Function3[T1, T2, T3, R], t1: Term, t2: Term, t3: Term): BodyElem
  def T[T1,T2,T3,T4,R](r: Variable, f: Function4[T1, T2, T3, T4, R], t1: Term, t2: Term, t3: Term, t4: Term): BodyElem
  def T[T1,T2,T3,T4,T5,R](r: Variable, f: Function5[T1, T2, T3, T4, T5, R], t1: Term, t2: Term, t3: Term, t4: Term, t5: Term): BodyElem

  def solve(): Unit

  def loadFactsFromFile(filename: String, relations: Map[String, Relation]): Unit
}

object API {
  def apply(): API = new APIImpl
}

