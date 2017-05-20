package latmap

import org.scalatest.FunSuite

class SolverTest extends FunSuite {
  test("Cross01") {
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()
      val dist = relation(2)
      dist(1, 2) :- ()
      dist(2, 3) :- ()
      dist(x, z) :- (dist(x, y), dist(y, z))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross02") {
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()
      val dist = relation(2)
      dist(1, 2) :- ()
      dist(2, 3) :- ()
      dist(3, 4) :- ()
      dist(4, 5) :- ()
      dist(x, z) :- (dist(x, y), dist(y, z))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross03"){
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
      dist(3, 1) :- ()
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross04"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()
      val c = variable()
      val A = relation(3)
      A(x, c, z) :- (A(x, c, y), A(y, c, z))
      A(1, "a", 2) :- ()
      A(2, "a", 3) :- ()
      A(1, "b", 2) :- ()
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross05"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()
      val c = variable()
      val A = relation(3)
      A(1, "a", 2) :- ()
      A(2, "a", 3) :- ()
      A(3, "b", 1) :- ()
      A(x, c, z) :- (A(x, c, y), A(y, c, z))
      A(x, "b", y) :- (A(x, "a", y))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross06"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()
      val c = variable()
      val A = relation(3)
      A(1, "a", 2) :- ()
      A(2, "a", 3) :- ()
      A(3, "b", 1) :- ()
      A(x, c, y) :- A(y, c, x)
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross08"){
    val p = API()

    {
      import p._
      val x = variable()
      val A = relation(1)
      val B = relation(1)
      val R = relation(1)
      A(1) :- ()
      A(2) :- ()
      B(2) :- ()
      B(3) :- ()
      R(x) :- (A(x), B(x))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross09"){
    val p = API()

    {
      import p._
      val x = variable()
      val A = relation(1)
      val B = relation(1)
      val C = relation(1)
      val R = relation(1)

      A(1) :- ()
      A(2) :- ()
      A(3) :- ()

      B(2) :- ()
      B(3) :- ()

      C(3) :- ()

      R(x) :- (A(x), B(x), C(x))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross10"){
    val p = API()

    {
      import p._
      val x = variable()
      val A = relation(1)
      val B = relation(1)
      val C = relation(1)
      val R = relation(1)

      A(1) :- ()
      A(2) :- ()
      A(3) :- ()

      B(2) :- ()
      B(3) :- ()

      C(3) :- ()

      R(x) :- (C(x), B(x), A(x), B(x), B(x), C(x), A(x))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross11"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()

      val A = relation(2)
      val B = relation(2)
      val R = relation(2)

      A(1, 2) :- ()
      A(3, 4) :- ()

      B(2, 3) :- ()
      B(4, 5) :- ()

      R(x, z) :- (A(x, y), B(y, z))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross12"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()
      val w = variable()

      val A = relation(2)
      val B = relation(2)
      val C = relation(2)
      val R = relation(2)

      A(1, 2) :- ()
      A(3, 4) :- ()

      B(2, 3) :- ()
      B(4, 5) :- ()

      C(3, 7) :- ()

      R(x, w) :- (A(x, y), B(y, z), C(z, w))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross13"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()

      val A = relation(2)
      val B = relation(2)
      val C = relation(2)
      val R = relation(2)

      A(1, 2) :- ()
      A(7, 1) :- ()

      B(2, 3) :- ()
      B(3, 5) :- ()

      C(3, 4) :- ()
      C(5, 6) :- ()

      C(x, z) :- (A(x, y), B(y, z))
      A(x, z) :- (C(x, y), B(y, z))
      B(x, z) :- (A(x, y), C(y, z))

    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Cross14"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()

      val A = relation(1)
      val B = relation(1)
      val C = relation(1)
      val R = relation(1)

      A(1) :- ()
      A(2) :- ()

      C(1) :- ()
      C(2) :- ()

      R(x) :- (A(x), B(x), C(x))
    }
    // TODO: Wildcard support? testWildcard01

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Lattice01"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()

      val A = relation(1, ParityLattice)

      A(1, ParityLattice.Odd) :- ()
      A(2, ParityLattice.Even) :- ()
      A(3, ParityLattice.Top) :- ()
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Lattice02"){
    val p = API()

    {
      import p._
      val x = variable()

      val A = relation(1, ParityLattice)

      A(1, ParityLattice.Odd) :- ()
      A(1, ParityLattice.Even) :- ()
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Lattice03"){
    val p = API()

    {
      import p._
      val x = variable()

      val A = relation(1, ParityLattice)

      A(1, ParityLattice.Odd) :- ()
      A(2, ParityLattice.Even) :- ()

      A(3, x) :- A(1, x)
      A(3, x) :- A(2, x)
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("NotEqual01"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()

      val A = relation(1)
      val B = relation(2)

      A(1) :- ()
      A(2) :- ()
      A(3) :- ()
      A(4) :- ()
      def neq(x : Int, y : Int) : Boolean = x != y
      B(x, y) :- (A(x), A(y), F(neq, x, y))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("NotEqual02"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()

      val A = relation(2)
      val B = relation(2)

      A(1, 2) :- ()
      A(2, 2) :- ()

      def neq(x : Int, y : Int) : Boolean = x != y
      B(x, y) :- (A(x, y), F(neq, x, y))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("NotEqual03"){
    val p = API()

    {
      import p._
      val x = variable()
      val y = variable()
      val z = variable()

      val A = relation(2)
      val B = relation(2)

      A(1, 2) :- ()
      A(2, 1) :- ()
      A(2, 3) :- ()
      def neq(x : Int, y : Int) : Boolean = x != y

      B(x, z) :- (A(x, y), A(y, z), F(neq, x, z))
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
  test("Transfer01"){
    val p = API()

    {
      import p._

      val A = relation(1)
      def f() : Int = 42
      // TODO: how to translate this one
    }

    println(p.asInstanceOf[APIImpl].program)

    p.solve()
  }
}
