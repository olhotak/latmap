package latmap

import scala.collection.mutable
import scala.collection.mutable.WrappedArray
import java.util.Arrays

class SimpleLatMap[T <: Lattice](val lattice: T) extends LatMap[T] {
  val rows = mutable.Map.empty[WrappedArray[Int], lattice.Elem]

  override val arity: Int = 5

  override def get(keys: Array[Int]): lattice.Elem = {
    rows.get(keys) match {
      case Some(e) => e
      case None => lattice.bottom
    }
  }

  override def keyIterator: Iterator[Array[Int]] = {
    rows.keysIterator.map(_.toArray)
  }

  override def put(keys: Array[Int], elem: lattice.Elem): Boolean = {
    rows.put(Arrays.copyOf(keys, keys.length), elem).contains(elem)
  }

  val indexes: mutable.ListBuffer[Index] = mutable.ListBuffer.empty[Index]

  def addIndex(index: Index): Unit = {
    indexes += index
  }
}
