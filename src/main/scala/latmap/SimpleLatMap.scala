package latmap

import scala.collection.mutable
import java.util.Arrays
import java.util.concurrent.ConcurrentHashMap
import java.util.function.BiFunction
import scala.collection.mutable.WrappedArray
import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec

final class SimpleLatMap[T <: Lattice](val lattice: T, val arity: Int) extends LatMap[T] {
//val rows = mutable.Map.empty[mutable.WrappedArray[Int], lattice.Elem]
  val rows = new ConcurrentHashMap[mutable.WrappedArray[Int], lattice.Elem]
  val addQueue = new ConcurrentLinkedQueue[(mutable.WrappedArray[Int], lattice.Elem)]
  val indexAddQueue = new ConcurrentLinkedQueue[mutable.WrappedArray[Int]]

  override def get(keys: Array[Int]): lattice.Elem = {
    val k: mutable.WrappedArray[Int] = keys
    rows.get(k) match {
      case null => lattice.bottom
      case x    => x
    }
  }

  override def keyIterator: Iterator[Array[Int]] = {
    val keys = rows.keys()
    new Iterator[Array[Int]] {
      def hasNext = keys.hasMoreElements()
      def next = keys.nextElement().array
    }
  }
  
  // Can NOT be called concurrently
  override def betweenWritePhases(): Unit = {
      val writes = indexAddQueue.size()
      indexes.foreach(_.prepareForWrites(writes))
  }
  
  // Can be called by multiple threads on the same latmap at the same time
  @tailrec override def writePhase1(): Unit = {
      val write = addQueue.poll()
      if (write != null)
      {
          val keys = write._1
          val elem = write._2
          var shouldAdd = true
          rows.merge(keys, elem, new BiFunction[lattice.Elem, lattice.Elem, lattice.Elem] {
              def apply(a: lattice.Elem, b: lattice.Elem) = {
                  shouldAdd = false
                  lattice.lub(a, b)
              }
          })
          if (shouldAdd) {
              indexAddQueue.add(keys)
          }
          writePhase1()
      }
  }
  
  @tailrec override def writePhase2(): Unit = {
      val write = indexAddQueue.poll()
      if (write != null)
      {
          indexes.foreach(_.put(write.array))
          writePhase2()
      }
  }

  override def put(keys: Array[Int], elem: lattice.Elem): Option[lattice.Elem] = {
    // TODO: The return value may not make sense,
    // since it does not take into account the concurrent adds.
    addQueue.add(keys, elem)
    val oldElem = rows.get(keys)
    val newElem = lattice.lub(elem, oldElem)
    
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
