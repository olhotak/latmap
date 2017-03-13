package latmap

case class Plan(planElements: PlanElement)

trait EvalContext {
  val translator: Translator

  val keyRegs: Array[Int]
  val latRegs: Array[Any]
}

trait PlanElement {
  val next: PlanElement
  def |>(f: PlanElement => PlanElement): PlanElement = f(this)
  def go(evalContext: EvalContext): Unit
}

/**
  * Performs a scan over the provided index, calling next.go() once for every result in the index.
  */
case class IndexScan(index: Index,
                     mergeLat: Boolean,
                     inputRegs: Array[Int],
                     outputRegs: Array[Int],
                     outputLatReg: Int,
                     next: PlanElement) extends PlanElement {

  def go(evalContext: EvalContext) = {
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

      if (outputLatReg >= 0) {
        var newLat = latticeMap.get(outputs).asInstanceOf[latticeMap.lattice.Elem]
        if (mergeLat)
          newLat = latticeMap.lattice.glb(newLat, evalContext.latRegs(outputLatReg).asInstanceOf[latticeMap.lattice.Elem])
        if (newLat != latticeMap.lattice.bottom)
          evalContext.latRegs(outputLatReg) = newLat
      }

      next.go(evalContext)
    }
  }
}

case class TransferFnArray(inputReg: Array[Int],
                           outputReg: Int,
                           function: Array[Any] => Any,
                           next: PlanElement) extends PlanElement {
  def go(evalContext: EvalContext) = {
    // TODO
    //      val input = if(inputReg >= 1000) latRegs(inputReg-1000) else i2f(keyRegs(inputReg))
    val output = function(???)
    if (outputReg >= 1000)
      evalContext.latRegs(outputReg-1000) = output
    else
      evalContext.keyRegs(outputReg) = evalContext.translator.toInt(output)
    next.go(evalContext)
  }
}

case class TransferFn1(inputReg: Int,
                       outputReg: Int,
                       function: Function1[Any, Any],
                       next: PlanElement) extends PlanElement {
  def go(evalContext: EvalContext) = {
    val input = if (inputReg >= 1000)
      evalContext.latRegs(inputReg-1000)
    else
      evalContext.translator.fromInt(evalContext.keyRegs(inputReg))
    val output = function(input)
    if (outputReg >= 1000)
      evalContext.latRegs(outputReg-1000) = output
    else
      evalContext.keyRegs(outputReg) = evalContext.translator.toInt(output)
  }
}
case class FilterFn1(next: PlanElement) extends PlanElement {
  def go(evalContext: EvalContext) = {
    ???
  }
}
case class WriteToLatMap() extends PlanElement {
  val next = null
  def go(evalContext: EvalContext) = {
    println(evalContext.keyRegs.mkString(" "))
    println(evalContext.latRegs.mkString(" "))
  }
}
