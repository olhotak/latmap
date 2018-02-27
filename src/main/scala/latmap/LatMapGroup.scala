package latmap

import scala.collection.mutable.ArrayBuffer

class LatMapQueue(name: String) {
  override def toString: String = name
  val keys = new ArrayBuffer[Array[Int]]()
  val values = new ArrayBuffer[Object]()
  def put(k: Array[Int], v: Object): Unit = {
    keys.append(k)
    values.append(v)
  }
  def keyIterator = keys.iterator
  def valueIterator = values.iterator
  def numFacts = keys.size
}
class LatMapGroup(val width: Int, val lattice: Lattice, name: String) {
  override def toString: String = name
  var iterationCount = 0
  val trueLatMap: AbstractLatMap = if(lattice == BoolLattice) new BoolLatMap(width) else new GeneralLatMap(width, lattice)
  var inputLatMap = new LatMapQueue(name+":"+iterationCount)
  iterationCount += 1
  var outputLatMap = new LatMapQueue(name+":"+iterationCount)

  def setInput(): Unit = {
    iterationCount += 1
    inputLatMap = outputLatMap
    outputLatMap = new LatMapQueue(name+":"+iterationCount)
  }
}
