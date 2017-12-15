package latmap

import scala.collection.mutable.ArrayBuffer

/**
  * Created by neerajensritharan on 2017-06-01.
  */
sealed trait LatMapType

class LatMapQueue(val lattice: Lattice, name: String) {
  override def toString: String = name
  val keys = new ArrayBuffer[Array[Int]]()
  val values = new ArrayBuffer[lattice.Elem]()
  def put(k: Array[Int], v: lattice.Elem): Unit = {
    keys.append(k)
    values.append(v)
  }
  def keyIterator = keys.iterator
  def valueIterator = values.iterator
  def numFacts = keys.size
}
class LatMapGroup(val arity: Int, val lattice: Lattice, name: String) {
  override def toString: String = name
  var iterationCount = 0
  val trueLatMap = new SimpleLatMap(lattice, arity, name+":true")
  var inputLatMap = new LatMapQueue(lattice, name+":"+iterationCount)
  iterationCount += 1
  var outputLatMap = new LatMapQueue(lattice, name+":"+iterationCount)

  def setInput(): Unit = {
    iterationCount += 1
    inputLatMap = outputLatMap
    outputLatMap = new LatMapQueue(lattice, name+":"+iterationCount)
  }
}
