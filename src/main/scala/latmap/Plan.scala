package latmap

/**
  * A Plan simply holds a pointer to the root PlanElement.
  */
case class Plan(planElements: PlanElement)

/**
  * Holds the current evaluation context, including registers for keys and lattice elements
  * and the translator. Passed in to each PlanElement as the only parameter to go().
  */
trait EvalContext {
  val translator: Translator

  val keyRegs: Array[Int]
  val latRegs: Array[Any]

  // These are only for saving and retrieving untranslated objects.
  def readFromReg(reg: Int): Any = {
    if (reg >= 1000)
      latRegs(reg - 1000)
    else
      translator.fromInt(keyRegs(reg))
  }
  def writeToReg(reg: Int, value: Any): Unit = {
    if (reg >= 1000)
      latRegs(reg - 1000) = value
    else
      keyRegs(reg) = translator.toInt(value)
  }
}

/**
  * A PlanElement acts on the evaluation context and calls go() on the next PlanElement a number of times.
  */
trait PlanElement {
  var next: PlanElement = null
  def go(evalContext: EvalContext): Unit
}

// TODO: Could just use same set of registers for input and output
case class KeyScan(index: Index,
                   inputKeyRegs: Array[Int],
                   outputKeyRegs: Array[Int]) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    val latticeMap = index.latticeMap
    val keys = new Array[Int](latticeMap.arity)
    var i = 0

    while (i < latticeMap.arity) {
      keys(i) = evalContext.keyRegs(inputKeyRegs(i))
      i += 1
    }
    val iterator = index.get(keys)
    while (iterator.hasNext) {
      val outputs = iterator.next
      i = 0
      while (i < outputKeyRegs.length) {
        i += 1
        evalContext.keyRegs(outputKeyRegs(i)) = outputs(i)
      }
      next.go(evalContext)
    }
  }
}

/**
  * Plan element that performs a scan over the provided index.
  * For each result in the index,
  *   writes the keys and lattice element to the provided output registers
  *   and calls next.go().
  *
  * @param index index to scan *
  * @param mergeLat whether or not to merge the lattice element
  * @param inputRegs indices of the input registers. must hold keys.
  * @param outputRegs indices of the output registers.
  * @param outputLatReg index of the output lattice register.
  */
case class IndexScan(index: Index,
                     mergeLat: Boolean,
                     inputRegs: Array[Int],
                     outputRegs: Array[Int],
                     outputLatReg: Int) extends PlanElement {

  def go(evalContext: EvalContext): Unit = {
    val latticeMap: index.latticeMap.type = index.latticeMap
    val keys = new Array[Int](latticeMap.arity)
    var i = 0

    i = 0
    while (i < latticeMap.arity) {
      if (inputRegs(i) >= 0)
        keys(i) = evalContext.keyRegs(inputRegs(i))
      i += 1
    }

    val iterator = index.get(keys)
    while (iterator.hasNext) {
      val outputs = iterator.next

      // Write to output registers
      i = 0
      while (i < outputRegs.length) {
        evalContext.keyRegs(outputRegs(i)) = outputs(i)
        i = i + 1
      }

      if (outputLatReg >= 0) { // TODO: why is this check here?
        var newLat = latticeMap.get(outputs)
        if (mergeLat)
          newLat = latticeMap.lattice.glb(newLat, evalContext.latRegs(outputLatReg - 1000).asInstanceOf[latticeMap.lattice.Elem])
        evalContext.latRegs(outputLatReg - 1000) = newLat
      }

      next.go(evalContext)
    }
  }
}

/**
  * PlanElement that produces a new value by calling the provided function
  * on input from the input registers.
  *
  * @param inputRegs
  * @param outputReg
  * @param function
  */
case class TransferFnArray(inputRegs: Array[Int],
                           outputReg: Int,
                           function: Array[Any] => Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    val input = new Array[Any](inputRegs.length)
    var i = 0

    i = 0
    while (i < inputRegs.length) {
      input(i) = evalContext.readFromReg(inputRegs(i))
      i += 1
    }

    val output = function(input)
    evalContext.writeToReg(outputReg, output)
    next.go(evalContext)
  }
}

case class TransferFn1(inputReg: Int,
                       outputReg: Int,
                       function: Any => Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    val input =
      if (inputReg >= 1000)
        evalContext.latRegs(inputReg - 1000)
      else
        evalContext.translator.fromInt(evalContext.keyRegs(inputReg))

    val output = function(input)
    if (outputReg >= 1000)
      evalContext.latRegs(outputReg - 1000) = output
    else
      evalContext.keyRegs(outputReg) = evalContext.translator.toInt(output)
  }
}
case class FilterFn1(inputReg: Int,
                     function: Any => Boolean) extends PlanElement {
  def go(evalContext: EvalContext) = {
    if ((inputReg >= 1000 && function(evalContext.latRegs(inputReg - 1000))) ||
        function(evalContext.translator.fromInt(evalContext.keyRegs(inputReg)))) {
      next.go(evalContext)
    }
  }
}

/**
  * Writes a (key, value) pair specified by (inputRegs, inputLatReg) to
  * the provided LatMap.
  */
case class WriteToLatMap[T <: Lattice](inputRegs: Array[Int],
                                       inputLatReg: Int,
                                       outputLatMap: LatMap[T]) extends PlanElement {
  require(inputRegs.length == outputLatMap.arity)
  def go(evalContext: EvalContext) = {
    outputLatMap.put(inputRegs.map(evalContext.keyRegs(_)),
      evalContext.latRegs(inputLatReg - 1000).asInstanceOf[outputLatMap.lattice.Elem])
  }
}
