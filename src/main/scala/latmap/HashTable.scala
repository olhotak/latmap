package latmap

final class HashTable(val width: Int, val positions: Array[Int]) {

  def hash(row: Array[Int]): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = arraySeed
    var i = 0
    while(i<positions.length) {
      h = mix(h, row(positions(i)))
      i += 1
    }
    finalizeHash(h, i) & Integer.MAX_VALUE
  }

  var table: Array[Array[Int]] = Array.ofDim[Array[Int]](10000019)

  def allocateRow: Array[Int] = Array.ofDim[Int](width)

  def find(row: Array[Int]): Int = {
    var rowNum = hash(row) % table.length
    while((table(rowNum) ne null) && !Util.equalRows(row, table(rowNum), positions)) {
      rowNum += 1
      rowNum %= table.length
    }
    rowNum
  }

  def contains(row: Array[Int]): Boolean = {
    table(find(row)) ne null
  }

  def insert(row: Array[Int]): Int = {
    val rowNum = find(row)
    if(table(rowNum) eq null) {
      table(rowNum) = row
      size += 1
      assert(size*2 < table.length)
      rowNum
    } else {
      rowNum | HashTable.EXISTING
    }
  }

  final class Iterator extends scala.collection.Iterator[Array[Int]] {
    var rowNum = 0
    private def seek(): Unit = {
      while (rowNum < table.length && (table(rowNum) eq null)) rowNum += 1
    }
    seek()

    def hasNext: Boolean = rowNum < table.length
    def next(): Array[Int] = {
      val ret = table(rowNum)
      rowNum += 1
      seek()
      ret
    }
  }
  def iterator = new Iterator

  var size = 0
}
object HashTable {
  final val EXISTING = Integer.MIN_VALUE
  def isExisting(insertRet: Int): Boolean = (insertRet & EXISTING) != 0
  def getRowNum(insertRet: Int): Int = insertRet & (~EXISTING)
}

object Util {
  def allKeys(width: Int): Array[Int] = (0 until width).toArray
  def equalRows(row1: Array[Int], row2: Array[Int], positions: Array[Int]): Boolean = {
    var i = 0
    while(i < positions.length) {
      if(row1(positions(i)) != row2(positions(i))) return false
      i += 1
    }
    return true
  }
}

import Util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class AbstractLatMap(val width: Int) {
  val table = new HashTable(width, allKeys(width))
  def size: Int = table.size
  def rowIterator: table.Iterator = table.iterator

  val indices: mutable.ListBuffer[Index] = mutable.ListBuffer.empty[Index]
  def addIndex(index: Index): Unit = indices += index
  def updateIndices(row: Array[Int]): Unit = indices.foreach(_.put(row))
  def selectIndex(boundVars: Set[Int]): Index = {
    if(boundVars.size == width) allKeyIndex
    else indices.find(_.positions.toSet == boundVars).getOrElse {
      val ret = new NaiveIndex(this, boundVars.toArray)
      indices += ret
      ret
    }
  }
  def dump(translator: Translator): Unit
  def allKeyIndex: Index
}

final class BoolLatMap(width: Int) extends AbstractLatMap(width) {
  def get(row: Array[Int]): Boolean = table.contains(row)
  def put(row: Array[Int]): Boolean = {
    val ret = !HashTable.isExisting(table.insert(row))
    if(ret) updateIndices(row)
    ret
  }
  override def allKeyIndex = new BoolAllKeyIndex(this)
  def dump(translator: Translator): Unit = {
    for(row <- table.iterator) println(row.map(translator.fromInt).mkString(","))
  }
}

final class GeneralLatMap(width: Int, val lattice: Lattice) extends AbstractLatMap(width) {
  val latElems: Array[AnyRef] = Array.ofDim[AnyRef](table.table.length)
  def get(row: Array[Int]): Any = {
    val rowNum = table.find(row)
    if(table.table(rowNum) eq null) lattice.bottom
    else latElems(rowNum)
  }
  def put(row: Array[Int], latElem: Any): AnyRef = {
    assert(latElem != lattice.bottom)
    val insertRet = table.insert(row)
    if(!HashTable.isExisting(insertRet)) {
      latElems(insertRet) = latElem.asInstanceOf[AnyRef]
      updateIndices(row)
      latElem.asInstanceOf[AnyRef]
    } else {
      val rowNum = HashTable.getRowNum(insertRet)
      val lub = lattice.lub(latElems(rowNum).asInstanceOf[lattice.Elem], latElem.asInstanceOf[lattice.Elem])
      if(lub != latElems(rowNum)) {
        latElems(rowNum) = lub.asInstanceOf[AnyRef]
        lub.asInstanceOf[AnyRef]
      } else {
        null
      }
    }
  }
  override def allKeyIndex = new GeneralAllKeyIndex(this)
  def dump(translator: Translator): Unit = {
    for(row <- table.iterator) println(row.map(translator.fromInt).mkString(","))
  }
}

final class HashIndex(val latMap: AbstractLatMap, val positions: Array[Int]) extends Index {
  latMap.addIndex(this)
  val table = new HashTable(latMap.width, positions)
  val rowLists: Array[ArrayBuffer[Array[Int]]] = Array.ofDim(table.table.length)

  override def get(row: Array[Int]): Iterator[Array[Int]] = {
    val rowNum = table.find(row)
    if(rowLists(rowNum) eq null) Iterator.empty
    else rowLists(rowNum).iterator
  }

  override def put(row: Array[Int]): Unit = {
    val rowNum = HashTable.getRowNum(table.insert(row))
    if(rowLists(rowNum) eq null) {
      rowLists(rowNum) = new ArrayBuffer[Array[Int]]()
    }
    rowLists(rowNum).append(row)
  }
  override def toString: String = s"HashIndex $latMap($positionString)"
}

final class NaiveIndex(val latMap: AbstractLatMap, val positions: Array[Int]) extends Index {
  override def get(row: Array[Int]): Iterator[Array[Int]] = {
    final class Iterator extends scala.collection.Iterator[Array[Int]] {
      val iterator = latMap.rowIterator
      var cur: Array[Int] = null
      @tailrec def bump(): Unit = {
        if(!iterator.hasNext) cur = null
        else {
          cur = iterator.next()
          if(!equalRows(row, cur, positions)) bump()
        }
      }
      bump()

      def hasNext: Boolean = cur ne null
      def next(): Array[Int] = {
        val ret = cur
        bump()
        ret
      }
    }
    new Iterator
  }

  override def toString: String = s"NaiveIndex $latMap($positionString)"
}

final class BoolAllKeyIndex(val latMap: BoolLatMap) extends Index {
  val positions = (0 until latMap.width).toArray
  override def get(row: Array[Int]): Iterator[Array[Int]] = {
    if(latMap.get(row)) Iterator.single(row) else Iterator.empty
  }
  override def toString: String = s"BoolAllKeyIndex $latMap($positionString)"
}

final class GeneralAllKeyIndex(val latMap: GeneralLatMap) extends Index {
  val positions = (0 until latMap.width).toArray
  override def get(row: Array[Int]): Iterator[Array[Int]] = {
    if(latMap.table.contains(row)) Iterator.single(row) else Iterator.empty
  }
  override def toString: String = s"GeneralAllKeyIndex $latMap($positionString)"
}
