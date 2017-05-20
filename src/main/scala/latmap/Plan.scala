package latmap

/**
  * A Plan simply holds a pointer to the root PlanElement.
  */
case class Plan(planElements: PlanElement, rule: Rule) {
  def go (_translator : Translator): Unit = {
    val evalContext = new EvalContext {
      override val keyRegs: Array[Int] = new Array[Int](rule.numKeyVars)
      override val latRegs: Array[Any] = new Array[Any](rule.numLatVars)
      override val translator: Translator = _translator
    }

    planElements.go(evalContext)
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
  * Const Plan Element 1: Adds constants to EvalContext and calls go() once
  */
case class KeyConstantEval(keyReg : Int,
                       const : Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    // TODO: How do i convert variable to index?
    evalContext.keyRegs(keyReg) = evalContext.translator.toInt(const)
    next.go(evalContext)
  }
}

/**
  * Constant Plan Element 2: Calls go() on all evalContexts that satisfy constant constraint
  */
case class KeyConstantFilter(keyReg : Int,
                        const : Any) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {

    if (evalContext.keyRegs(keyReg) == evalContext.translator.toInt(const)) {
      next.go(evalContext)
    }
  }
}
// TODO: I combined the two

case class LatConstantEval(latReg : Int, const : Any, lattice : Lattice) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    evalContext.latRegs(latReg - 1000) = const
    next.go(evalContext)
  }
}

case class LatConstantFilter(latReg : Int, const : Any, lattice : Lattice) extends PlanElement {
  def go(evalContext: EvalContext): Unit = {
    val newLat : lattice.Elem = lattice.glb(const.asInstanceOf[lattice.Elem],
          evalContext.latRegs(latReg - 1000).asInstanceOf[lattice.Elem])
    evalContext.latRegs(latReg - 1000) = newLat
    if (newLat != lattice.bottom)
      next.go(evalContext)
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
    val latticeMap: LatMap[_ <: Lattice] = index.latticeMap
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
        val lattice: Lattice = latticeMap.lattice
        if (mergeLat)
          newLat = latticeMap.lattice.glb(newLat, evalContext.latRegs(outputLatReg - 1000).asInstanceOf[latticeMap.lattice.Elem])
        evalContext.latRegs(outputLatReg - 1000) = newLat
      }

      next.go(evalContext)
    }
  }
}
case class InputPlanElement(outputRegs: Array[Int],
                            outputLatReg: Int,
                            latmapGroup : LatMapGroup
                           ) extends PlanElement {
  def go(evalContext: EvalContext) : Unit = {
    val latticeMap : LatMap[_ <: Lattice] = latmapGroup.inputLatMap
    var i = 0

    val iterator = latticeMap.keyIterator
    while (iterator.hasNext) {
      val outputs = iterator.next

      // Write to output registers
      i = 0
      while (i < outputRegs.length) {
        evalContext.keyRegs(outputRegs(i)) = outputs(i)
        i = i + 1
      }

      if (outputLatReg >= 0) { // TODO: why is this check here?
        val newLat = latticeMap.get(outputs)
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
case class WriteToLatMap(inputRegs: Array[Int],
                         inputLatReg: Int,
                         latmapGroup : LatMapGroup,
                         constRule : Boolean) extends PlanElement {
  require(inputRegs.length == latmapGroup.get(True).arity)
  def go(evalContext: EvalContext) = {
    val trueLatMap = latmapGroup.trueLatMap
    val outputLatMap = latmapGroup.outputLatMap
    val putVal = trueLatMap.put(inputRegs.map(evalContext.keyRegs(_)),
        evalContext.latRegs(inputLatReg - 1000).asInstanceOf[trueLatMap.lattice.Elem])

    putVal match {
      case None =>
        if (constRule) {
          outputLatMap.put(inputRegs.map(evalContext.keyRegs(_)),
          evalContext.latRegs(inputLatReg - 1000).asInstanceOf[outputLatMap.lattice.Elem])
          println(s"Writing ${inputRegs.map((i) => evalContext.translator.fromInt(evalContext.keyRegs(i)))mkString(" ")} ->" +
            s" ${evalContext.latRegs(inputLatReg - 1000).asInstanceOf[outputLatMap.lattice.Elem]}" + " to :" + outputLatMap)
        }
      case Some(elem) =>
        outputLatMap.put(inputRegs.map(evalContext.keyRegs(_)), elem.asInstanceOf[outputLatMap.lattice.Elem])
        println(s"Writing ${inputRegs.map((i) => evalContext.translator.fromInt(evalContext.keyRegs(i)))mkString(" ")} ->" +
          s" ${evalContext.latRegs(inputLatReg - 1000).asInstanceOf[outputLatMap.lattice.Elem]}" + " to :" + outputLatMap)
    }

    if (next != null)
      next.go(evalContext)
  }
}
