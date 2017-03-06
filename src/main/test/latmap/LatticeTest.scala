package latmap

import org.scalatest._
import Matchers._

class LatticeTest extends FunSuite {
  test("dist lattice makes sense") {
    // Upper bound of two numbers is their minimum.
    val lat = DistLattice
    def Dst(x: Int): DistLattice.Elem = DistLattice.Dst(x)
    lat.leq(Dst(3), Dst(4)) shouldBe false
    lat.leq(Dst(4), Dst(2)) shouldBe true
    lat.glb(Dst(5), Dst(6)) shouldBe Dst(6)
    lat.lub(Dst(5), Dst(6)) shouldBe Dst(5)
    lat.bottom shouldBe DistLattice.Infinity
  }

  test("two point lattice makes sense") {
    val lat = TwoPointLattice
    lat.leq(lat.Top, lat.Bot) shouldBe false
    lat.leq(lat.Top, lat.Top) shouldBe true
    lat.leq(lat.Bot, lat.Top) shouldBe true
    lat.glb(lat.Top, lat.Bot) shouldBe lat.Bot
    lat.lub(lat.Top, lat.Bot) shouldBe lat.Top
    lat.bottom shouldBe lat.Bot
  }
}
