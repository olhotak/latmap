package latmap

class NaiveIndex[T <: Lattice](
        val latticeMap: LatMap[T],
        val positions: Set[Int]) extends Index {
    
    override def get(keys: Array[Int]): Iterator[Array[Int]] = {
        for {
            entry <- latticeMap.keyIterator
            if positions.forall(x => keys(x) == entry(x))
        } yield entry
    }
    
    override def put(keys: Array[Int]) = false
}
