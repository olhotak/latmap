package latmap

class Solver {
  def solve(program: Program) = {
    // TODO: create a plan for each pair (Rule, Atom position in the rule)
    val planner = new Planner()
    var keyVarMap = scala.collection.mutable.Map[program.Variable, KeyVariable]()
    var latVarMap = scala.collection.mutable.Map[program.Variable, LatVariable]()
    var latMapMap = scala.collection.mutable.Map[LatMap[_ <: Lattice], LatMapGroup]()
    var constVariables = scala.collection.mutable.ListBuffer.empty[program.Const]

    var keyId : Int = 0
    var latId : Int = 0
    def generateKeyVariable(v : program.Variable): Unit = {
      if (!keyVarMap.contains(v)) {
        keyVarMap(v) = KeyVariable("key: " + keyId)
        keyId += 1
      }
    }
    def generateLatVariable(v : program.Variable): Unit = {
      if (!latVarMap.contains(v)) {
        latVarMap(v) = LatVariable("lat: " + latId)
        latId += 1
      }
    }

    // map all program Variables to Latmap Variables
    program.rules.foreach((rule) => {

      // head elements
      rule.head.keyVars.foreach((v) =>
          generateKeyVariable(v)
      )
      if (!latVarMap.contains(rule.head.latVar))
        generateLatVariable(rule.head.latVar)

      // body elements
      rule.body.foreach((bodyElem) => {
        bodyElem match {
          case s: program.Const =>
          case s: program.Atom =>
            s.keyVars.foreach((v) =>
                generateKeyVariable(v)
            )
            generateLatVariable(s.latVar)
        }
      })
    })


    // Convert all Program.Rules to Latmap.Rules
    val backendRules : Seq[Rule] = program.rules.map((rule) =>
        Rule(
          new LatmapRuleElement(
            latMapMap.getOrElseUpdate(rule.head.latMap, new LatMapGroup(rule.head.latMap)),
            rule.head.keyVars.map((pv) => keyVarMap(pv)) :+ latVarMap(rule.head.latVar),
            rule.body.forall((e) => e.isInstanceOf[program.Const])
          ),
          rule.body.map((ruleElement) =>
            ruleElement match {
              case const: program.Const =>
                if (keyVarMap.contains(const.variable))
                  new KeyConstantRuleElement(
                    keyVarMap(const.variable),
                    const.constant
                  )
                else
                  new LatConstantRuleElement(latVarMap(const.variable), const.constant, rule.head.latMap.lattice)
              case atom: program.Atom =>
                new LatmapRuleElement(
                  latMapMap.getOrElseUpdate(atom.latMap, new LatMapGroup(atom.latMap)),
                  atom.keyVars.map((pv) => keyVarMap(pv)) :+ latVarMap(atom.latVar),
                  false
                )
            }
          ).toList
        )
    )
    val groupLatMaps : Seq[LatMapGroup] = latMapMap.values.toSeq

    val constPlans: Seq[(Rule,Plan)] = backendRules
      .filter((rule) => rule.bodyElements.forall(!_.isInstanceOf[LatmapRuleElement]))
      .map((rule) => (rule, planner.plan(rule)))

    val regPlans : Map[LatMapGroup,Seq[(Rule, Plan)]] = backendRules
      .flatMap(
        (rule) => rule.bodyElements.collect({
          case l : LatmapRuleElement => (l.latmapGroup, (rule, planner.plan(rule, Some(rule.bodyElements.indexOf(l)))))
        })
      ).groupBy(_._1).map { case (k,v) => (k,v.map(_._2))}

    val myTranslator = new Translator()
    def printFacts(latmapType : LatMapType) : Unit = {
      println("Printing " + latmapType)
      groupLatMaps.foreach((g) => {
        println("latmap: " + g.get(latmapType))
        g.get(latmapType).keyIterator.foreach((key) => {
          val out = key.map((i) => myTranslator.fromInt(i)).mkString(" ") + " : " + g.get(latmapType).get(key)
          println(out)
        })
        println()
        println()

      })
      println()
    }
    constPlans.foreach((tup) => {
      val rule : Rule = tup._1
      val plan : Plan = tup._2

      plan.go(myTranslator)
    })
    var newFacts : Int = groupLatMaps.map(_.outputLatMap.numFacts()).sum

    while (newFacts  > 0){
      // swap inputLatMaps with outputLatMaps
      groupLatMaps.foreach(_.setInput())
      printFacts(latmap.Input)
      for (latmapGroup : LatMapGroup <- groupLatMaps if latmapGroup.inputLatMap.numFacts() > 0){
        regPlans.getOrElse(latmapGroup, Seq()).foreach((tup) => {
          val plan: Plan = tup._2
          plan.go(myTranslator)

        })
      }
      printFacts(latmap.Output)

      newFacts = groupLatMaps.map(_.outputLatMap.numFacts()).sum
    }
    printFacts(latmap.True)

  }
}
