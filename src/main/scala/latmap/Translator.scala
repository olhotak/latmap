package latmap

import scala.collection.mutable

class Translator {
  val toIntMap = new mutable.HashMap[Any, Int]
  val fromIntMap = new mutable.ArrayBuffer[Any]

  def toInt(x: Any): Int = {
    this.synchronized {
      if (toIntMap.contains(x)) {
        toIntMap(x)
      } else {
        val result = fromIntMap.size
        fromIntMap += x
        toIntMap(x) = result
        result
      }
    }
  }

  def fromInt(x: Int): Any = {
    if (x < fromIntMap.size) {
      fromIntMap(x)
    }
  }
}
