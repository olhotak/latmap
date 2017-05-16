package latmap

trait Program {
  trait ProgVariable
  trait ProgBodyElem
  trait ProgAtom extends ProgBodyElem {
    def latMap: LatMap[_ <: Lattice]
    def keyVars: Seq[Variable]
    def latVar: Variable
  }
  trait ProgConst extends ProgBodyElem {
    def variable: Variable
    def constant: Any
  }
  trait ProgRule {
    def head: Atom
    def body: Seq[BodyElem]
  }

  type Variable <: ProgVariable
  type BodyElem <: ProgBodyElem
  type Atom <: ProgAtom
  type Const <: ProgConst
  type Rule <: ProgRule

  def rules: Seq[Rule]
}
