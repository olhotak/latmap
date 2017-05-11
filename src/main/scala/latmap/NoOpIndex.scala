package latmap

class NoOpIndex(val latticeMap: LatMap[_ <: Lattice], val positions: Set[Int]) extends Index {
  def put(keys: Array[Int]) = Unit
  
  def get(keys: Array[Int]) = List().iterator
  
  def prepareForWrites(number: Int) = Unit
}