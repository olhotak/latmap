package latmap

import org.scalatest.FunSuite

class PlanTest extends FunSuite {
  test("IndexScan basic functionality") {
    val lattice = DistLattice
    val latMap = new SimpleLatMap(lattice, 2)
    val outputLatMap = new SimpleLatMap(lattice, 2)
    val index: Index = new NaiveIndex(latMap, Set(1))
    val myTranslator: Translator = new Translator()

    implicit def to_i(x: String): Int = myTranslator.toInt(x)

    latMap.addIndex(index)
    latMap.put(Array("x", "y"), lattice.Dst(5))
    latMap.put(Array("y", "z"), lattice.Dst(6))
    latMap.put(Array("z", "a"), lattice.Dst(3))
    latMap.put(Array("y", "a"), lattice.Dst(4))

    val step0 = IndexScan(index, mergeLat = false, Array(0, 1), Array(0, 1), 1000)
    val step1 = FilterFn1(0, _.asInstanceOf[String].startsWith("z"))
    val step2 = TransferFnArray(Array(0), 0, (_) => "y")
    val step3 = WriteToLatMap(Array(0, 1), 1000, outputLatMap)
    step0.next = step1
    step1.next = step2
    step2.next = step3

    val evalContext = new EvalContext {
      override val latRegs: Array[Any] = new Array(10)
      override val translator: Translator = myTranslator
      override val keyRegs: Array[Int] = Array(0, "a", 0, 0, 0, 0, 0, 0, 0, 0)
    }

    step0.go(evalContext)
    outputLatMap.flushWrites()
  }
}
