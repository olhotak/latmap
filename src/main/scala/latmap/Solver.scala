package latmap

class Solver {
  def solve(program: Program): Unit = {
    val planner = new Planner(program)
    val keyVarMap = scala.collection.mutable.Map[program.KeyVariable, KeyVariable]()
    val latVarMap = scala.collection.mutable.Map[program.LatVariable, LatVariable]()

    var id = 0
    def newId(): Int = { id = id + 1; id }
    def convertKeyVariable(k: program.KeyVariable): KeyVariable =
      keyVarMap.getOrElseUpdate(k, KeyVariable(newId()))
    def convertLatVariable(l: program.LatVariable): LatVariable =
      latVarMap.getOrElseUpdate(l, LatVariable(newId(), l.lattice))
    def convertVariable(v : program.Variable): Variable = v match {
      case k: program.KeyVariable => convertKeyVariable(k)
      case l: program.LatVariable => convertLatVariable(l)
    }
    def convertVariables(v : Seq[program.Variable]) : Seq[Variable] = v.map(convertVariable)


    // Convert all Program.Rules to Latmap.Rules
    val backendRules : Seq[Rule] = program.rules.map((rule) =>
        Rule(
          new LatmapRuleElement(
            rule.head.latMapGroup,
            convertVariables(rule.head.keyVars) :+ convertVariable(rule.head.latVar),
            rule.body.forall((e) => e.isInstanceOf[program.Const])
          ),
          rule.body.map {
            case const: program.Const => const.variable match {
              case k: program.KeyVariable => new KeyConstantRuleElement(
                convertKeyVariable(k),
                const.constant
              )
              case l: program.LatVariable => new LatConstantRuleElement(
                convertLatVariable(l),
                const.constant,
                l.lattice
              )
            }
            case atom: program.Atom =>
              new LatmapRuleElement(
                atom.latMapGroup,
                convertVariables(atom.keyVars) :+ convertVariable(atom.latVar),
                false
              )
            case function: program.Function =>
              new FunctionRuleElement(function.function, convertVariables(function.arguments), convertVariable(function.result))

          }.toList
        )
    )
    val constPlans: Seq[(Rule,Plan)] = backendRules
      .filter((rule) => rule.bodyElements.forall(!_.isInstanceOf[LatmapRuleElement]))
      .map((rule) => (rule, planner.plan(rule)))

    val regPlans : Map[LatMapGroup,Seq[(Rule, Plan)]] = backendRules
      .flatMap(
        (rule) => rule.bodyElements.collect({
          case l : LatmapRuleElement => (l.latmapGroup, (rule, planner.plan(rule, Some(rule.bodyElements.indexOf(l)))))
        })
      ).groupBy(_._1).map { case (k,v) => (k,v.map(_._2))}

    val allPlans = constPlans.map{case (rule,plan) => plan} ++ regPlans.flatMap{case (lmg, seq) => seq.map{case (rule, plan) => plan}}

    constPlans.foreach((tup) => {
      val rule : Rule = tup._1
      val plan : Plan = tup._2

      plan.go()
    })

    var done = false

    while (!done) {
      done = true
      // swap inputLatMaps with outputLatMaps
      for (latmapGroup: LatMapGroup <- program.latMapGroups) {
        latmapGroup.clearInput()
        latmapGroup.trueLatMap match {
          case blm: BoolLatMap =>
            val keys = latmapGroup.outputLatMap.keyIterator
            while(keys.hasNext) {
              val row = keys.next()
              val putVal = blm.put(row)
              if (putVal) latmapGroup.inputLatMap.put(row, null)
            }
          case glm: GeneralLatMap =>
            val keys = latmapGroup.outputLatMap.keyIterator
            val values = latmapGroup.outputLatMap.valueIterator
            while(keys.hasNext) {
              val row = keys.next()
              val value = values.next()
              val putVal = glm.put(row, value)
              if (putVal ne null) latmapGroup.inputLatMap.put(row, putVal)
            }
        }
        latmapGroup.clearOutput()
      }
      for (latmapGroup: LatMapGroup <- program.latMapGroups if latmapGroup.inputLatMap.numFacts > 0) {
        regPlans.getOrElse(latmapGroup, Seq()).foreach { case (rule, plan) =>
          done = false
          plan.go()
        }
      }
    }

    val timings = allPlans.map{plan => (plan, plan.timer.elapsedTime)}.sortBy{case (plan, timer) => timer}
    for((plan, time) <- timings) {
      println(f"${time/1e9d}%1.3f\n")
      println(plan)
      println()
    }
  }
}
