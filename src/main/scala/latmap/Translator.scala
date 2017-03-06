package latmap

import scala.collection.mutable

class Translator {
  val toMap = new mutable.HashMap[Any, Int]
  val fromMap = new mutable.ArrayBuffer[Any]
  def toInt(x: Any): Int = {
    if (toMap.contains(x)) {
      toMap(x)
    } else {
      val result = fromMap.size
      fromMap += x
      toMap(x) = result
      result
    }
  }
  def fromInt(x: Int): Any = {
    if (x < fromMap.size) {
      fromMap(x)
    }
  }
}
