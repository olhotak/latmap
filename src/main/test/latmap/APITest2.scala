package latmap

object APITest2 extends App {
  val p = API()

  {
    import p._
    val x = variable()
    val l = variable()
    val one = variable()
    val two = variable()
    val three = variable()
    val A = relation(1, ParityLattice)
    """lat A(x: Int, v: Parity)
      |
      |A(1, Parity.Odd).
      |A(2, Parity.Even).
      |
      |A(3, x) :- A(1,x).
      |A(3, x) :- A(2,x).
    """
    //not sure how to translate this one
    //A(x, l) :- (x := 1, l := ParityLattice.Odd)
    //A(x, l) :- (x := 2, l := ParityLattice.Even)
    //dist(x, y) :- (x := 2, y := 3)
    A(three, l) :- (three := 3, x := 1, A(x, l))

  }

  p.solve()
}
