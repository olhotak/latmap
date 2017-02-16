package latmap

trait Plan {
  val i2f: Int=>Any
  val f2i: Any=>Int

  val keyRegs: Array[Int]
  val latRegs: Array[Any]

  trait PlanElement {
    val next: PlanElement
    def go(): Unit
  }
  trait IndexScan extends PlanElement {
    val index: Index
    val inputRegs: Array[Int]
    val outputRegs: Array[Int]
    val outputLatReg: Int
    val mergeLat: Boolean

    def go() = {
      val latticeMap = index.latticeMap
      val lattice: latticeMap.lattice.type = latticeMap.lattice
      val keys = new Array[Int](latticeMap.arity)
      var i = 0

      i = 0
      while(i < latticeMap.arity) {
        if(inputRegs(i) >= 0) keys(i) = keyRegs(inputRegs(i))
        i = i + 1
      }

      val iterator = index.get(keys)
      while(iterator.hasNext) {
        val outputs = iterator.next

        i = 0
        while(i < outputRegs.length) {
          keyRegs(outputRegs(i)) = outputs(i)
        }

        if(outputLatReg >= 0) {
          var newLat = latticeMap.get(outputs)
          if(mergeLat) newLat = latticeMap.lattice.glb(newLat, latRegs(outputLatReg).asInstanceOf[latticeMap.lattice.Elem])
          latRegs(outputLatReg) = newLat
        }

        next.go()
      }

    }
  }
  trait TransferFn1 extends PlanElement {
    val inputReg: Int
    val outputReg: Int
    val function: Function1[Any,Any]

    def go() = {
      val input = if(inputReg >= 1000) latRegs(inputReg-1000) else i2f(keyRegs(inputReg))
      val output = function(input)
      if(outputReg >= 1000) latRegs(outputReg-1000) = output else keyRegs(outputReg) = f2i(output)
    }
  }
  trait FilterFn1 extends PlanElement {
    // TODO
  }
  trait WriteToLatMap extends PlanElement {
    // TODO
  }
}
