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
    dist(1, 2) :- ()
    dist(2, 3) :- ()
    def filter(x: Int, y: Int) = true
    dist(x, z) :- (dist(x, y), dist(y, z), F(filter, x, z))
    def transfer(x: Int, y: Int) = x+y
    dist(x, z) :- (dist(x, y), T(z, transfer, x, y))
  }

  println(p.asInstanceOf[APIImpl].program)

  p.solve()
}
