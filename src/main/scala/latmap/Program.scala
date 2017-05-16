package latmap

trait Program {
  trait ProgVariable
  type Variable <: ProgVariable
  trait ProgBodyElem
  type BodyElem <: ProgBodyElem
  trait ProgAtom extends ProgBodyElem {
    def latMap: LatMap[_ <: Lattice]
    def keyVars: Seq[Variable]
    def latVar: Variable
  }
  type Atom <: ProgAtom
  trait ProgConst extends ProgBodyElem {
    def variable: ProgVariable
    def constant: Any
  }
  type Const <: ProgConst
  trait ProgRule {
    def head: Atom
    def body: Seq[BodyElem]
  }
  type Rule <: ProgRule
  def rules: Seq[Rule]
}
