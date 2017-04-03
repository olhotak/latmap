package latmap

/**
  * External API.
  */
// TODO: review this interface
// can a head atom have more than one transfer fn?
// can a body atom have more than one filter fn?
// can a body atom have transfer fns?
// why are fns not terms?
trait Term
case class KeyVar(keyVar: String) extends Term
case class LatVar(latVar: String) extends Term
case class Key(value: AnyRef) extends Term
case class Elem(value: Any) extends Term
case class FilterFn[T](fn: T => Boolean) extends Term
case class TransferFn[T](fn: T, params: List[LatVar]) extends Term

// TODO: should the public-facing atoms/rules be separate from those defined in Rule.scala?
case class FlixAtom(name: String, terms: List[Term])
case class FlixRule(head: FlixAtom, body: List[FlixAtom])

class Solver {
  def solve(flixRules: List[FlixRule]) = {
    // Step 1: Transform flixRules into a list of Rules, with appropriate planElement methods
    // Step 2: Figure out which flixRules are facts and write them to the appropriate LatMaps
    // Step 3:
  }
}
