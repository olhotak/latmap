package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers

object TestUtils extends FunSuite with Matchers {
  def testKeysEqual(a: TraversableOnce[Array[Int]], b: TraversableOnce[Array[Int]]) = {
    val p = a.map(_.toList).toList
    val q = b.map(_.toList).toList
    
    val x = p.groupBy(identity)
    val y = q.groupBy(identity)
    
    withClue(s"LHS: $p\nRHS: $q\n") {
        x shouldEqual y
    }
  }
}