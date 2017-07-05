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

  trait BodyElem extends APIBodyElem

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
    def :-(atoms: BodyElem*): Unit = {
      val constVars = mutable.Map[Any, Variable]()

      def convertTerm(term: Term): program.Variable = {
        term match {
          case Variable(id) => program.Variable(id)
          case Constant(c) => convertTerm(constVars.getOrElseUpdate(c, variable()))
        }
      }

      def convertAtom(atom: Atom): program.Atom =
        program.Atom(atom.latMap, atom.keyTerms map convertTerm, convertTerm(atom.latTerm))

      def convertBodyElem(elem: BodyElem): program.BodyElem = elem match {
        case atom: Atom => convertAtom(atom)
        case Const(v, c) => program.Const(convertTerm(v), c)
      }

      def constants: Seq[program.BodyElem] = constVars.map{case (c, v) => program.Const(convertTerm(v), c)}.toSeq

      program.rules += program.Rule(convertAtom(this), (atoms map convertBodyElem) ++ constants)
    }
  }

  case class Const(variable: Term, constant: Any) extends BodyElem

  case class Constant(constant: Any) extends Term

  override implicit def anyConst(c: Any): Constant = Constant(c)

  def solve() = new Solver().solve(program)
}

