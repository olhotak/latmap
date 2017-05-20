package latmap

/**
  * Created by neerajensritharan on 2017-06-01.
  */
sealed trait LatMapType
case object Input extends LatMapType
case object Output extends LatMapType
case object True extends LatMapType

class LatMapGroup(_trueLatMap: LatMap[_ <: Lattice]) {

  val trueLatMap = _trueLatMap
  var inputLatMap = new SimpleLatMap(trueLatMap.lattice, trueLatMap.arity)
  var outputLatMap = new SimpleLatMap(trueLatMap.lattice, trueLatMap.arity)

  def get (latmapType : LatMapType) : LatMap[_ <: Lattice] = latmapType match {
    case Input => inputLatMap
    case Output => outputLatMap
    case True => trueLatMap
  }

  def setInput(): Unit = {
    inputLatMap = outputLatMap
    outputLatMap = new SimpleLatMap(trueLatMap.lattice, trueLatMap.arity)
  }

}
