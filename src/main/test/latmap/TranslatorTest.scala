package latmap

import org.scalatest.FunSuite
import org.scalatest.Matchers

class TranslatorTest extends FunSuite with Matchers {
  test("translator works") {
    val translator = new Translator
    val lat = TwoPointLattice

    val bot_int = translator.toInt(TwoPointLattice.bottom)
    bot_int shouldEqual translator.toInt(TwoPointLattice.bottom)
    val cake_int = translator.toInt("cake")

    cake_int should not equal bot_int
    cake_int shouldEqual translator.toInt("cake")

    TwoPointLattice.bottom shouldEqual translator.fromInt(bot_int)
    "cake" shouldEqual translator.fromInt(cake_int)
  }
}
