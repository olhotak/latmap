package latmap

/**
  * Created by neerajensritharan on 2017-07-13.
  */

import scala.collection.mutable
import java.util.Arrays

class SimpleLatMap[T <: Lattice](val lattice: T, val arity : Int) extends LatMap[T] {
  val rows = mutable.Map.empty[mutable.WrappedArray[Int], lattice.Elem]

  override def get(keys: Array[Int]): lattice.Elem = {
    rows.get(keys) match {
      case Some(e) => e
      case None => lattice.bottom
    }
  }

  override def keyIterator: Iterator[Array[Int]] = {
    rows.keysIterator.map(_.toArray)
  }

  /**
    *put(keys : Array[Int], elem : lattice.Elem) : Any
    * value
    * map.get(keys) match {
    * None =>
    *   map.put(v)
    *   updateIndexes(keys)
    *   v
    * Some(old) =>
    *   if (new != old)
    *     map.put(v lub old)
    *     v lub old
    *   else
    *     null
    *
    */
  override def put(keys: Array[Int], elem: lattice.Elem): Option[lattice.Elem] = {
    rows.get(keys) match {
      case None =>
        rows.put(Arrays.copyOf(keys, keys.length), elem)
        indexes.foreach(_.put(keys))
        Some(elem)
      case Some(old) =>
        if (elem != old){
          val newLatElem = lattice.lub(elem, get(keys))
          rows.put(Arrays.copyOf(keys, keys.length), newLatElem)
          Some(newLatElem)
        }
        else
          None
    }
  }

  val indexes: mutable.ListBuffer[Index] = mutable.ListBuffer.empty[Index]

  def addIndex(index: Index): Unit = {
    indexes += index
  }

  override def numFacts() : Int = rows.size

  def writePhase1(): Unit = {}
  def betweenWritePhases(): Unit = {}
  def writePhase2(): Unit = {}
}
