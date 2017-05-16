package latmap

object APITest extends App {
  val p = API()

  {
    import p._
    val x = variable()
    val y = variable()
    val z = variable()
    val dist = relation(2)
    dist(x, z) :- (dist(x, y), dist(y, z))
    dist(x, y) :- (x := 1, y := 2)
    dist(x, y) :- (x := 2, y := 3)
  }

  p.solve()
}
