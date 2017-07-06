package latmap

import javax.swing.ProgressMonitor

import scala.collection.mutable

class ProgramImpl extends Program {
  case class Rule(head: Atom, body: Seq[BodyElem]) extends ProgRule {
    override def toString = head + " :- " + body.mkString(", ")
  }
  case class Variable(id: Int) extends ProgVariable {
    override def toString = "v" + id
  }
  trait BodyElem extends ProgBodyElem
  case class Atom(latMap: LatMap[_ <: Lattice], keyVars: Seq[Variable], latVar: Variable) extends ProgAtom with BodyElem {
    override def toString = latMap + keyVars.mkString("(", ", ", ")") + "[" + latVar + "]"
  }
  case class Const(variable: Variable, constant: Any) extends ProgConst with BodyElem {
    override def toString = variable + " := " + constant
  }
  case class Filter(function: AnyRef, arguments: Seq[Variable]) extends ProgFilter with BodyElem {
    override def toString = function + arguments.mkString("(", ", ", ")")
  }
  case class Transfer(result: Variable, function: AnyRef, arguments: Seq[Variable]) extends ProgTransfer with BodyElem {
    override def toString = result + " := " + function + arguments.mkString("(", ", ", ")")
  }

  val rules = mutable.ArrayBuffer[Rule]()
  override def toString = rules.mkString("\n")
}

class APIImpl extends API {
  val program = new ProgramImpl

  case class Variable(id: Int) extends APIVariable {
    def :=(constant: Any) = Const(this, constant)
  }

  var nextId = 0
  def variable(): Variable = {
    nextId = nextId + 1
    Variable(nextId)
  }

  case class Relation(arity: Int, lattice: Lattice) extends APIRelation {
    val latMap = new SimpleLatMap(lattice, arity)
    def apply(vars: Term*): Atom = {
      val newVars = if(lattice eq BoolLattice) vars :+ Constant(BoolLattice.top) else vars
      Atom(latMap, newVars.dropRight(1), newVars.last)
    }
  }

  def relation(arity: Int, lattice: Lattice): Relation = {
    new Relation(arity, lattice)
  }

  def relation(arity: Int): Relation = {
    new Relation(arity, BoolLattice)
  }

  trait BodyElem extends APIBodyElem {
    def convert(termToVar: Term=>program.Variable): program.BodyElem
  }

  def constantAtoms(atoms: Seq[BodyElem]): Seq[Const] = {
    val terms = atoms.flatMap(_ match {
      case Atom(_, keyVars, latVar) => keyVars :+ latVar
      case _ => Nil
    })
    terms.flatMap(_ match {
      case ct@Constant(c) => List(Const(ct, c))
      case _ => Nil
    }).distinct
  }

  case class Atom(latMap: LatMap[_ <: Lattice], keyTerms: Seq[Term], latTerm: Term) extends BodyElem with APIAtom {
    override def convert(termToVar: (Term) => program.Variable): program.Atom =
      program.Atom(latMap, keyTerms map termToVar, termToVar(latTerm))

    def :-(atoms: BodyElem*): Unit = {
      val constVars = mutable.Map[Any, Variable]()

      def convertTerm(term: Term): program.Variable = term match {
        case Variable(id) => program.Variable(id)
        case Constant(c) => convertTerm(constVars.getOrElseUpdate(c, variable()))
      }

      def constants: Seq[program.BodyElem] = constVars.map{case (c, v) => program.Const(convertTerm(v), c)}.toSeq

      program.rules += program.Rule(this.convert(convertTerm),
        (atoms map {atom => atom.convert(convertTerm)}) ++ constants)
    }
  }

  case class Const(variable: Term, constant: Any) extends BodyElem {
    override def convert(termToVar: (Term) => program.Variable): program.Const =
      program.Const(termToVar(variable), constant)
  }

  case class Constant(constant: Any) extends Term

  override implicit def anyConst(c: Any): Constant = Constant(c)

  case class Filter(function: AnyRef, arguments: Seq[Term]) extends BodyElem {
    override def convert(termToVar: (Term) => program.Variable): program.Filter =
      program.Filter(function, arguments map termToVar)
  }
  def F(f: Function0[Boolean]): BodyElem = Filter(f, Seq())
  def F[T1](f: Function1[T1, Boolean], t1: Term): BodyElem = Filter(f, Seq(t1))
  def F[T1,T2](f: Function2[T1, T2, Boolean], t1: Term, t2: Term): BodyElem = Filter(f, Seq(t1, t2))
  def F[T1,T2,T3](f: Function3[T1, T2, T3, Boolean], t1: Term, t2: Term, t3: Term): BodyElem = Filter(f, Seq(t1, t2, t3))
  def F[T1,T2,T3,T4](f: Function4[T1, T2, T3, T4, Boolean], t1: Term, t2: Term, t3: Term, t4: Term): BodyElem = Filter(f, Seq(t1, t2, t3, t4))
  def F[T1,T2,T3,T4,T5](f: Function5[T1, T2, T3, T4, T5, Boolean], t1: Term, t2: Term, t3: Term, t4: Term, t5: Term): BodyElem = Filter(f, Seq(t1, t2, t3, t4, t5))


  case class Transfer(result: Variable, function: AnyRef, arguments: Seq[Term]) extends BodyElem {
    override def convert(termToVar: (Term) => program.Variable): program.Transfer =
      program.Transfer(termToVar(result), function, arguments map termToVar)
  }
  def T[R](r: Variable, f: Function0[R]): BodyElem = Transfer(r, f, Seq())
  def T[T1,R](r: Variable, f: Function1[T1, R], t1: Term): BodyElem = Transfer(r, f, Seq(t1))
  def T[T1,T2,R](r: Variable, f: Function2[T1, T2, R], t1: Term, t2: Term): BodyElem = Transfer(r, f, Seq(t1, t2))
  def T[T1,T2,T3,R](r: Variable, f: Function3[T1, T2, T3, R], t1: Term, t2: Term, t3: Term): BodyElem = Transfer(r, f, Seq(t1, t2, t3))
  def T[T1,T2,T3,T4,R](r: Variable, f: Function4[T1, T2, T3, T4, R], t1: Term, t2: Term, t3: Term, t4: Term): BodyElem = Transfer(r, f, Seq(t1, t2, t3, t4))
  def T[T1,T2,T3,T4,T5,R](r: Variable, f: Function5[T1, T2, T3, T4, T5, R], t1: Term, t2: Term, t3: Term, t4: Term, t5: Term): BodyElem = Transfer(r, f, Seq(t1, t2, t3, t4, t5))

  def solve() = new Solver().solve(program)
}

