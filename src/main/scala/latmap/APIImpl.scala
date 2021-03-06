package latmap

import javax.swing.ProgressMonitor

import scala.collection.mutable
import scala.io.Source

class ProgramImpl extends Program {
  case class Rule(head: Atom, body: Seq[BodyElem]) extends ProgRule {
    override def toString = head + " :- " + body.mkString(", ")
  }
  abstract class BaseVariable(id: Int) extends Variable {
    override def toString = "v" + id
  }
  case class KeyVariableImpl(id: Int) extends BaseVariable(id) with KeyVariable {
  }
  case class LatVariableImpl(id: Int, lattice: Lattice) extends BaseVariable(id) with LatVariable {
  }
  trait BodyElem extends ProgBodyElem
  case class Atom(latMapGroup: LatMapGroup, keyVars: Seq[KeyVariable], latVar: LatVariable) extends ProgAtom with BodyElem {
    override def toString = latMapGroup + keyVars.mkString("(", ", ", ")") + "[" + latVar + "]"
  }
  case class Const(variable: Variable, constant: Any) extends ProgConst with BodyElem {
    override def toString = variable + " := " + constant
  }
  case class Function(result: Variable, function: AnyRef, arguments: Seq[Variable]) extends ProgFunction with BodyElem {
    override def toString = result + " := " + function + arguments.mkString("(", ", ", ")")
  }

  def addIndex(latMapGroup: LatMapGroup, indices: Set[Int]): Unit = new HashIndex(latMapGroup.trueLatMap, indices.toArray.sorted)

  val rules = mutable.ArrayBuffer[Rule]()
  val latMapGroups = mutable.Set[LatMapGroup]()
  override def toString = rules.mkString("\n")
  val translator = new Translator()
}

class APIImpl extends API {
  val program = new ProgramImpl

  abstract class Variable(id: Int) extends APIVariable {
    def :=(constant: Any) = Const(this, constant)
  }

  case class KeyVariable(id: Int) extends Variable(id) {
  }

  case class LatVariable(id: Int, lattice: Lattice) extends Variable(id) {
  }

  var nextId = 0
  def variable(): KeyVariable = {
    nextId = nextId + 1
    KeyVariable(nextId)
  }

  def latVariable(lattice: Lattice): LatVariable = {
    nextId = nextId + 1
    LatVariable(nextId, lattice)
  }

  class Relation(arity: Int, lattice: Lattice, name: String) extends APIRelation {
    val latMapGroup = new LatMapGroup(arity, lattice, name)
    def apply(vars: Term*): Atom = {
      val newVars = if(lattice eq BoolLattice) vars :+ Constant(BoolLattice.top) else vars
      Atom(latMapGroup, newVars.dropRight(1), newVars.last)
    }

    def numFacts: Int = {
      latMapGroup.trueLatMap.size
    }

    def dump(): Unit = latMapGroup.trueLatMap.dump(program.translator)
  }

  def relation(arity: Int, lattice: Lattice, name: String): Relation = {
    val ret = new Relation(arity, lattice, name)
    program.latMapGroups += ret.latMapGroup
    ret
  }

  trait BodyElem extends APIBodyElem {
    def convert(termToVar: (Term,Option[Lattice])=>program.Variable): program.BodyElem
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

  case class Atom(latMapGroup: LatMapGroup, keyTerms: Seq[Term], latTerm: Term) extends BodyElem with APIAtom {
    override def convert(termToVar: (Term, Option[Lattice]) => program.Variable): program.Atom =
      program.Atom(latMapGroup,
        (keyTerms map ((t) => termToVar(t, None))).map(_.asInstanceOf[program.KeyVariable]),
        termToVar(latTerm, Some(latMapGroup.lattice)).asInstanceOf[program.LatVariable])

    def :-(atoms: BodyElem*): Unit = {
      val constVars = mutable.Map[Any, Variable]()

      def convertTerm(term: Term, constLattice: Option[Lattice]): program.Variable = term match {
        case KeyVariable(id) => program.KeyVariableImpl(id)
        case LatVariable(id, lattice) => program.LatVariableImpl(id, lattice)
        case Constant(c) => constLattice match {
          case None => convertTerm(constVars.getOrElseUpdate(c, variable()), None)
          case Some(lattice) => convertTerm(constVars.getOrElseUpdate(c, latVariable(lattice)), None)
        }
      }

      def constants: Seq[program.BodyElem] = constVars.map{case (c, v) => program.Const(convertTerm(v, None), c)}.toSeq

      program.rules += program.Rule(this.convert(convertTerm),
        (atoms map {atom => atom.convert(convertTerm)}) ++ constants)
      //val newAtoms = if(latMap.lattice eq BoolLattice) atoms :+ Const(latVar, BoolLattice.top) else atoms
      //rules += Rule(this, newAtoms)
    }

    override def addIndex(terms: Term*): Unit = {
      val termsSet = terms.toSet
      val indices = keyTerms.zipWithIndex.flatMap {
        case (term, idx) => term match {
          case kv: KeyVariable if termsSet.contains(kv) => Seq(idx)
          case _ => Seq()
        }
      }.toSet

      program.addIndex(latMapGroup, indices)
    }
  }

  case class Const(variable: Term, constant: Any) extends BodyElem {
    override def convert(termToVar: (Term, Option[Lattice]) => program.Variable): program.Const =
    program.Const(termToVar(variable, None), constant)
  }

  case class Constant(constant: Any) extends Term

  override implicit def anyConst(c: Any): Constant = Constant(c)


  case class Function(result: Variable, function: AnyRef, arguments: Seq[Term]) extends BodyElem {
    override def convert(termToVar: (Term, Option[Lattice]) => program.Variable): program.Function =
      program.Function(termToVar(result, None), function, arguments map ((t) => termToVar(t, None)))
  }
  def T[R](r: Variable, f: Function0[R]): BodyElem = Function(r, f, Seq())
  def T[T1,R](r: Variable, f: Function1[T1, R], t1: Term): BodyElem = Function(r, f, Seq(t1))
  def T[T1,T2,R](r: Variable, f: Function2[T1, T2, R], t1: Term, t2: Term): BodyElem = Function(r, f, Seq(t1, t2))
  def T[T1,T2,T3,R](r: Variable, f: Function3[T1, T2, T3, R], t1: Term, t2: Term, t3: Term): BodyElem = Function(r, f, Seq(t1, t2, t3))
  def T[T1,T2,T3,T4,R](r: Variable, f: Function4[T1, T2, T3, T4, R], t1: Term, t2: Term, t3: Term, t4: Term): BodyElem = Function(r, f, Seq(t1, t2, t3, t4))
  def T[T1,T2,T3,T4,T5,R](r: Variable, f: Function5[T1, T2, T3, T4, T5, R], t1: Term, t2: Term, t3: Term, t4: Term, t5: Term): BodyElem = Function(r, f, Seq(t1, t2, t3, t4, t5))

  def F(f: Function0[Boolean]): BodyElem = Function(latVariable(BoolLattice), f, Seq())
  def F[T1](f: Function1[T1, Boolean], t1: Term): BodyElem = Function(latVariable(BoolLattice), f, Seq(t1))
  def F[T1,T2](f: Function2[T1, T2, Boolean], t1: Term, t2: Term): BodyElem = Function(latVariable(BoolLattice), f, Seq(t1, t2))
  def F[T1,T2,T3](f: Function3[T1, T2, T3, Boolean], t1: Term, t2: Term, t3: Term): BodyElem = Function(latVariable(BoolLattice), f, Seq(t1, t2, t3))
  def F[T1,T2,T3,T4](f: Function4[T1, T2, T3, T4, Boolean], t1: Term, t2: Term, t3: Term, t4: Term): BodyElem = Function(latVariable(BoolLattice), f, Seq(t1, t2, t3, t4))
  def F[T1,T2,T3,T4,T5](f: Function5[T1, T2, T3, T4, T5, Boolean], t1: Term, t2: Term, t3: Term, t4: Term, t5: Term): BodyElem = Function(latVariable(BoolLattice), f, Seq(t1, t2, t3, t4, t5))


  def solve() = new Solver().solve(program)

  def loadFactsFromFile(filename: String, relations: Map[String, Relation]): Unit = {
    val lines = Source.fromFile(filename).getLines().toList
    val fact = """(\w[\d\w]*)\((.*)\)\.""".r

    for (line <- lines) {
      line match {
        case fact(name, keys) =>
          val relation = relations(name)

          val terms = keys.trim()
            .drop(1)
            .dropRight(1)
            .split("""(?<!\\)\"\s*,\s*\"""")
            .map(c => Constant(c))
          relation.apply(terms:_*) :- ()
      }
    }
  }
}

