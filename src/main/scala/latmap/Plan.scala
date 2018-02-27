package latmap

class Plan(planElements: PlanElement, rule: Rule, translator: Translator, numKeyVars: Int, numLatVars: Int) {
  val evalContext = new EvalContext {
    override val keyRegs: Array[Int] = new Array[Int](numKeyVars)
    override val latRegs: Array[Any] = new Array[Any](numLatVars)
    override val translator: Translator = Plan.this.translator
  }
  def go(): Unit = {
    planElements.go(evalContext)
  }

  override def toString: String = rule + "\n" + planElements
}

object Plan {
  def readRegs(evalContext: EvalContext, row: Array[Int], regs: Array[Int]): Unit = {
    var i = 0
    while(i < regs.length) {
      row(i) = evalContext.keyRegs(regs(i))
      i += 1
    }
  }
  def readKeyRegs(evalContext: EvalContext, row: Array[Int], keys: Array[Int], regs: Array[Int]): Unit = {
    var i = 0
    while(i < keys.length) {
      row(keys(i)) = evalContext.keyRegs(regs(i))
      i += 1
    }
  }
  def writeRegs(evalContext: EvalContext, regs: Array[Int], row: Array[Int]): Unit = {
    var i = 0
    while(i < regs.length) {
      evalContext.keyRegs(regs(i)) = row(i)
      i += 1
    }
  }
  def writeKeyRegs(evalContext: EvalContext, regs: Array[Int], row: Array[Int], nonKeys: Array[Int]): Unit = {
    var i = 0
    while(i < nonKeys.length) {
      evalContext.keyRegs(regs(i)) = row(nonKeys(i))
      i += 1
    }
  }
}

/**
  * Holds the current evaluation context, including registers for keys and lattice elements
  * and the translator. Passed in to each PlanElement as the only parameter to go().
  */
trait EvalContext {
  val translator: Translator

  val keyRegs: Array[Int]
  val latRegs: Array[Any]
}

/**
  * A PlanElement acts on the evaluation context and calls go() on the next PlanElement a number of times.
  */
trait PlanElement {
  var next: PlanElement = null
  def go(evalContext: EvalContext): Unit
}

final class KeyConstantEval(reg: Int, const: Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    evalContext.keyRegs(reg) = evalContext.translator.toInt(const)
    next.go(evalContext)
  }

  override def toString: String = s"KeyConstantEval $const\n$next"
}

final class KeyConstantFilter(reg: Int, const: Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    if (evalContext.keyRegs(reg) == evalContext.translator.toInt(const)) {
      next.go(evalContext)
    }
  }

  override def toString: String = s"KeyConstantFilter $const\n$next"
}

final class LatConstantEval(reg: Int, const: Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    evalContext.latRegs(reg) = const
    next.go(evalContext)
  }

  override def toString: String = s"LatConstantEval $const\n$next"
}

final class GlbPlanElement(inOutReg: Int, otherReg: Int, lattice: Lattice) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    val newLat = lattice.glb(evalContext.latRegs(inOutReg).asInstanceOf[lattice.Elem], evalContext.latRegs(otherReg).asInstanceOf[lattice.Elem])
    evalContext.latRegs(inOutReg) = newLat
    if(newLat != lattice.bottom) next.go(evalContext)
  }

  override def toString: String = s"Glb\n$next"
}

abstract class AbstractIndexScan(index: Index, inputRegs: Array[Int], outputRegs: Array[Int])
  extends PlanElement {
  val latMap: AbstractLatMap = index.latMap
  val keys = index.positions
  assert(inputRegs.length == keys.length)
  assert(inputRegs.length + outputRegs.length == latMap.width)
  val nonKeys: Array[Int] = (0 until latMap.width).toSet.diff(keys.toSet).toSeq.sorted.toArray
  assert(keys.length + nonKeys.length == latMap.width)
  val row = latMap.table.allocateRow
}

final class BoolIndexScan(index: Index, inputRegs: Array[Int], outputRegs: Array[Int])
  extends AbstractIndexScan(index, inputRegs, outputRegs) {
  def go(evalContext: EvalContext): Unit = {
    Plan.readKeyRegs(evalContext, row, keys, inputRegs)

    val iterator = index.get(row)
    while (iterator.hasNext) {
      Plan.writeKeyRegs(evalContext, outputRegs, iterator.next(), nonKeys)
      next.go(evalContext)
    }
  }

  override def toString: String = s"BoolIndexScan $index ${inputRegs.mkString} ${outputRegs.mkString}\n$next"
}

final class GeneralIndexScan(index: Index, inputRegs: Array[Int], outputRegs: Array[Int], latReg: Int)
  extends AbstractIndexScan(index, inputRegs, outputRegs) {
  val generalLatMap = latMap.asInstanceOf[GeneralLatMap]
  def go(evalContext: EvalContext): Unit = {
    Plan.readKeyRegs(evalContext, row, keys, inputRegs)

    val iterator = index.get(row)
    while (iterator.hasNext) {
      val outRow = iterator.next()
      Plan.writeKeyRegs(evalContext, outputRegs, outRow, nonKeys)
      evalContext.latRegs(latReg) = generalLatMap.get(outRow)
      next.go(evalContext)
    }
  }

  override def toString: String = s"GeneralIndexScan $index ${inputRegs.mkString} ${outputRegs.mkString} $latReg\n$next"
}

final class BoolInputPlanElement(outputRegs: Array[Int], latMapGroup: LatMapGroup) extends PlanElement {
  def go(evalContext: EvalContext) : Unit = {
    val iterator = latMapGroup.inputLatMap.keyIterator
    while (iterator.hasNext) {
      Plan.writeRegs(evalContext, outputRegs, iterator.next())
      next.go(evalContext)
    }
  }

  override def toString: String = s"BoolInputPlanElement $latMapGroup${outputRegs.mkString("(",",",")")}\n$next"
}

final class GeneralInputPlanElement(outputRegs: Array[Int], latReg: Int, latMapGroup: LatMapGroup) extends PlanElement {
  def go(evalContext: EvalContext) : Unit = {
    val iterator = latMapGroup.inputLatMap.keyIterator
    val valIterator = latMapGroup.inputLatMap.valueIterator
    while (iterator.hasNext) {
      Plan.writeRegs(evalContext, outputRegs, iterator.next())
      evalContext.latRegs(latReg) = valIterator.next
      next.go(evalContext)
    }
  }

  override def toString: String = s"GeneralInputPlanElement $latMapGroup\n$next"
}

final class CastLatKey(keyReg: Int, latReg: Int) extends PlanElement {
  def go(evalContext: EvalContext) : Unit = {
    evalContext.keyRegs(keyReg) = evalContext.translator.toInt(evalContext.latRegs(latReg))
    next.go(evalContext)
  }

  override def toString: String = s"CastLatKey\n$next"

}

final class CastKeyLat(latReg: Int, keyReg: Int) extends PlanElement {
  def go(evalContext: EvalContext) : Unit = {
    evalContext.latRegs(latReg) = evalContext.translator.fromInt(evalContext.keyRegs(keyReg))
    next.go(evalContext)
  }

  override def toString: String = s"CastKeyLat $keyReg $latReg\n$next"

}

final class Function(inputRegs: Array[Int], outputReg: Int, function: Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    val result : Any = inputRegs.length match {
      case 0 => function.asInstanceOf[Function0[Any]]()
      case 1 => function.asInstanceOf[Function1[Any, Any]](evalContext.latRegs(inputRegs(0)))
      case 2 => function.asInstanceOf[Function2[Any, Any, Any]](evalContext.latRegs(inputRegs(0)), evalContext.latRegs(inputRegs(1)))
      case 3 => function.asInstanceOf[Function3[Any, Any, Any, Any]](evalContext.latRegs(inputRegs(0)), evalContext.latRegs(inputRegs(1)), evalContext.latRegs(inputRegs(2)))
      case 4 => function.asInstanceOf[Function4[Any, Any, Any, Any, Any]](evalContext.latRegs(inputRegs(0)), evalContext.latRegs(inputRegs(1)), evalContext.latRegs(inputRegs(2)), evalContext.latRegs(inputRegs(3)))
      case 5 => function.asInstanceOf[Function5[Any, Any, Any, Any, Any, Any]](evalContext.latRegs(inputRegs(0)), evalContext.latRegs(inputRegs(1)), evalContext.latRegs(inputRegs(2)), evalContext.latRegs(inputRegs(3)), evalContext.latRegs(inputRegs(4)))
    }
    evalContext.latRegs(outputReg) = result
    next.go(evalContext)
  }

  override def toString: String = s"$outputReg := Function(${inputRegs.mkString(", ")})\n$next"
}

final class WriteToGeneralLatMap(inputRegs: Array[Int], inputLatReg: Int, latMapGroup : LatMapGroup )
extends PlanElement {
  require(inputRegs.length == latMapGroup.width)
  def go(evalContext: EvalContext) = {
    val trueLatMap = latMapGroup.trueLatMap.asInstanceOf[GeneralLatMap]
    val row = trueLatMap.table.allocateRow
    Plan.readRegs(evalContext, row, inputRegs)
    val putVal = trueLatMap.put(row, evalContext.latRegs(inputLatReg))
    if(putVal ne null) latMapGroup.outputLatMap.put(row, putVal)
    if (next != null) next.go(evalContext)
  }

  override def toString: String = s"WriteToGeneralLatMap $latMapGroup${inputRegs.mkString("(",",",s") $inputLatReg")}\n$next"
}

final class WriteToBoolLatMap(inputRegs: Array[Int], latMapGroup : LatMapGroup ) extends PlanElement {
  require(inputRegs.length == latMapGroup.width)
  def go(evalContext: EvalContext) = {
    val trueLatMap = latMapGroup.trueLatMap.asInstanceOf[BoolLatMap]
    val row = trueLatMap.table.allocateRow
    Plan.readRegs(evalContext, row, inputRegs)
    val putVal = trueLatMap.put(row)
    if(putVal) latMapGroup.outputLatMap.put(row, null)
    if (next != null) next.go(evalContext)
  }

  override def toString: String = s"WriteToBoolLatMap $latMapGroup${inputRegs.mkString("(",",",")")}\n$next"
}

/**
  * Signals a dead end in a chain of PlanElements.
  *
  * Everything after this PlanElement is eliminated by planner (i.e. never run)
  */
final class DeadEnd() extends PlanElement {
  override def go(evalContext: EvalContext): Unit = {}
  override def toString: String = s"DeadEnd\n$next"
}

final class CheckBottom(reg: Int, lattice: Lattice) extends PlanElement {
  override def go(evalContext: EvalContext): Unit = {
    if(evalContext.latRegs(reg) != lattice.bottom) next.go(evalContext)
  }
  override def toString: String = s"CheckBottom $reg\n$next"
}
