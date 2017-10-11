package latmap

trait Program {
  trait ProgVariable {
    def lattice: Option[Lattice]
  }
  trait ProgBodyElem
  trait ProgAtom extends ProgBodyElem {
    def latMap: LatMap[_ <: Lattice]
    def keyVars: Seq[KeyVariable]
    def latVar: LatVariable
  }
  trait ProgConst extends ProgBodyElem {
    def variable: Variable
    def constant: Any
  }
  trait ProgRule {
    def head: Atom
    def body: Seq[BodyElem]
  }
  trait ProgFilter extends ProgBodyElem {
    def function: AnyRef // cast the function to FunctionN, where N is arguments.size
    def arguments: Seq[Variable]
  }
  trait ProgTransfer extends ProgBodyElem {
    def result: Variable
    def function: AnyRef // cast the function to FunctionN, where N is arguments.size
    def arguments: Seq[Variable]
  }

  type Variable <: ProgVariable
  type KeyVariable <: Variable
  type LatVariable <: Variable
  type BodyElem <: ProgBodyElem
  type Atom <: ProgAtom
  type Const <: ProgConst
  type Filter <: ProgFilter
  type Transfer <: ProgTransfer
  type Rule <: ProgRule

  def rules: Seq[Rule]
}
