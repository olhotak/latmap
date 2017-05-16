package latmap

import javax.swing.ProgressMonitor

import scala.collection.mutable

class APIImpl extends API with Program {
  case class Variable(id: Int) extends APIVariable with ProgVariable {
    def :=(constant: Any) = Const(this, constant)
  }

  var nextId = 0
  def variable(): Variable = {
    nextId = nextId + 1
    Variable(nextId)
  }

  case class Relation(arity: Int, lattice: Lattice) extends APIRelation {
    val latMap = new SimpleLatMap(lattice, arity)
    def apply(vars: Variable*): Atom = {
      val newVars = if(lattice eq BoolLattice) vars :+ variable() else vars
      Atom(latMap, newVars.dropRight(1), newVars.last)
    }
  }

  def relation(arity: Int, lattice: Lattice): Relation = {
    new Relation(arity, lattice)
  }

  def relation(arity: Int): Relation = {
    new Relation(arity, BoolLattice)
  }

  trait BodyElem extends APIBodyElem with ProgBodyElem

  case class Atom(latMap: LatMap[_ <: Lattice], keyVars: Seq[Variable], latVar: Variable) extends BodyElem with APIAtom with ProgAtom {
    def :-(atoms: BodyElem*): Unit = {
      rules += Rule(this, atoms)
    }
  }

  case class Const(variable: Variable, constant: Any) extends ProgConst with BodyElem

  case class Rule(head: Atom, body: Seq[BodyElem]) extends ProgRule

  val rules = mutable.ArrayBuffer[Rule]()

  def solve() = new Solver().solve(this)
}

