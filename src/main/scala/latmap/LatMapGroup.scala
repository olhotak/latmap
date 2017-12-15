package latmap

/**
  * Created by neerajensritharan on 2017-06-01.
  */
sealed trait LatMapType
case object Input extends LatMapType
case object Output extends LatMapType
case object True extends LatMapType

class LatMapGroup(arity: Int, val lattice: Lattice, name: String) {
  var iterationCount = 0
  val trueLatMap = new SimpleLatMap(lattice, arity, name+":true")
  var inputLatMap = new SimpleLatMap(lattice, arity, name+":"+iterationCount)
  iterationCount += 1
  var outputLatMap = new SimpleLatMap(lattice, arity, name+":"+iterationCount)

  def get (latmapType : LatMapType) : LatMap[_ <: Lattice] = latmapType match {
    case Input => inputLatMap
    case Output => outputLatMap
    case True => trueLatMap
  }

  def setInput(): Unit = {
    iterationCount += 1
    inputLatMap = outputLatMap
    outputLatMap = new SimpleLatMap(lattice, arity, name+":"+iterationCount)
  }

}
