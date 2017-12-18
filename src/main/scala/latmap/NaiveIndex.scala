package latmap

class NaiveIndex(
                  val latticeMap: LatMap[_ <: Lattice],
                  val positions: Set[Int]) extends Index {
    
  override def get(keys: Array[Int]): Iterator[Array[Int]] = {
    for {
      entry <- latticeMap.keyIterator
      if positions.forall(x => keys(x) == entry(x))
    } yield entry
  }
    
  override def put(keys: Array[Int]) = Unit
    
  override def prepareForWrites(number: Int) = Unit

  override def toString: String = s"NaiveIndex $latticeMap($positionString)"
}

class AllKeyIndex(val latticeMap: LatMap[_ <: Lattice]) extends Index {
  val positions = (0 until latticeMap.arity).toSet

  override def get(keys: Array[Int]): Iterator[Array[Int]] = {
    val elem = latticeMap.get(keys)
    if(elem == latticeMap.lattice.bottom) Iterator.empty
    else Iterator.single(keys)
  }

  override def put(keys: Array[Int]) = Unit

  override def prepareForWrites(number: Int) = Unit

  override def toString: String = s"AllKeyIndex $latticeMap($positionString)"
}
