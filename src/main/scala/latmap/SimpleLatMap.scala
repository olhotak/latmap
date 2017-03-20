package latmap

import scala.collection.mutable
import java.util.Arrays
import java.util.concurrent.ConcurrentHashMap
import java.util.function.BiFunction
import scala.collection.mutable.WrappedArray

class SimpleLatMap[T <: Lattice](val lattice: T) extends LatMap[T] {
//  val rows = mutable.Map.empty[mutable.WrappedArray[Int], lattice.Elem]
    val rows = new ConcurrentHashMap[mutable.WrappedArray[Int], lattice.Elem]

  override val arity: Int = 5

  override def get(keys: Array[Int]): lattice.Elem = {
    val k: mutable.WrappedArray[Int] = keys
    rows.get(k) match {
      case null => lattice.bottom
      case x => x
    }
  }

  override def keyIterator: Iterator[Array[Int]] = {
    val keys = rows.keys()
    new Iterator[Array[Int]] {
        def hasNext = keys.hasMoreElements()
        def next = keys.nextElement().array
    }
  }

  override def put(keys: Array[Int], elem: lattice.Elem): lattice.Elem = {
    indexes.foreach(_.put(keys))
    rows.merge(keys, elem, new BiFunction[lattice.Elem, lattice.Elem, lattice.Elem] {
        def apply(a: lattice.Elem, b: lattice.Elem) = lattice.lub(a, b)
    })
  }

  val indexes: mutable.ListBuffer[Index] = mutable.ListBuffer.empty[Index]

  def addIndex(index: Index): Unit = {
    indexes += index
  }
}
