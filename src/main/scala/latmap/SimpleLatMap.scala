package latmap

import scala.collection.mutable
import java.util.Arrays

class SimpleLatMap[T <: Lattice](val lattice: T, val arity: Int) extends LatMap[T] {
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

  override def put(keys: Array[Int], elem: lattice.Elem): Option[lattice.Elem] = {
    val oldElem = get(keys)
    val newElem = lattice.lub(elem, oldElem)
    indexes.foreach(_.put(keys))
    rows.put(Arrays.copyOf(keys, keys.length), newElem)
    if (lattice.leq(oldElem, newElem) && lattice.leq(newElem, oldElem))
      None
    else
      Some(newElem)
  }

  val indexes: mutable.ListBuffer[Index] = mutable.ListBuffer.empty[Index]

  def addIndex(index: Index): Unit = {
    indexes += index
  }
}
