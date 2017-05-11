package latmap

trait Program {
  trait ProgVariable
  type Variable <: ProgVariable
  trait ProgAtom {
    def latMap: LatMap[_]
    def keyVars: Seq[Variable]
    def latVar: Variable
  }
  type Atom <: ProgAtom
  trait ProgRule {
    def head: Atom
    def body: Seq[Atom]
  }
  type Rule <: ProgRule
  def rules: Seq[Rule]
}
