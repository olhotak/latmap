package latmap

import scala.annotation._, elidable._

class Solver {
  def solve(program: Program): Unit = {
    val planner = new Planner()
    var keyVarMap = scala.collection.mutable.Map[program.KeyVariable, KeyVariable]()
    var latVarMap = scala.collection.mutable.Map[program.LatVariable, LatVariable]()
    var constVariables = scala.collection.mutable.ListBuffer.empty[program.Const]
    var latMapGroups = Set[LatMapGroup]()

    var keyId : Int = 0
    var latId : Int = 0
    def generateKeyVariable(v : program.KeyVariable): Unit = {
      if (!keyVarMap.contains(v)) {
        keyVarMap(v) = KeyVariable("key: " + keyId)
        keyId += 1
      }
    }
    def generateLatVariable(v : program.LatVariable): Unit = {
      if (!latVarMap.contains(v)) {
        latVarMap(v) = LatVariable("lat: " + latId, v.lattice)
        latId += 1
      }
    }
    def convertVariable(v : program.Variable): Variable = v match {
      case k: program.KeyVariable => keyVarMap(k)
      case l: program.LatVariable => latVarMap(l)
    }
    def convertVariables(v : Seq[program.Variable]) : Seq[Variable] = v.map(convertVariable)
    // pass 1 : generate latmap.Variables for head elements and body atom elements
    program.rules.foreach((rule) => {

      // head elements
      rule.head.keyVars.foreach((v) =>
          generateKeyVariable(v)
      )
      if (!latVarMap.contains(rule.head.latVar))
        generateLatVariable(rule.head.latVar)

      // body elements
      rule.body.foreach {
        case s: program.Atom =>
          s.keyVars.foreach((v) =>
            generateKeyVariable(v)
          )
          generateLatVariable(s.latVar)
        case _ =>
      }
    })

    // pass 2 : generate key variables for all non-lat constants
    program.rules.foreach((rule) => {
      rule.body.foreach {
        case s: program.Const => s.variable match {
          case k: program.KeyVariable => generateKeyVariable(k)
          case _ =>
        }
        case _ =>
      }
    })


    // Convert all Program.Rules to Latmap.Rules
    val backendRules : Seq[Rule] = program.rules.map((rule) =>
        Rule(
          new LatmapRuleElement(
            rule.head.latMapGroup,
            rule.head.keyVars.map((pv) => keyVarMap(pv)) :+ latVarMap(rule.head.latVar),
            rule.body.forall((e) => e.isInstanceOf[program.Const])
          ),
          rule.body.map {
            case const: program.Const => const.variable match {
              case k: program.KeyVariable => new KeyConstantRuleElement(
                keyVarMap(k),
                const.constant
              )
              case l: program.LatVariable => new LatConstantRuleElement(
                latVarMap(l),
                const.constant,
                l.lattice
              )
            }
            case atom: program.Atom =>
              new LatmapRuleElement(
                atom.latMapGroup,
                atom.keyVars.map((pv) => keyVarMap(pv)) :+ latVarMap(atom.latVar),
                false
              )
            case filter: program.Filter =>
              new FilterFnRuleElement(filter.function,
                filter.arguments.map {
                  case k: program.KeyVariable => keyVarMap(k)
                  case l: program.LatVariable => latVarMap(l)
                }
              )
            case transfer: program.Transfer =>
              new TransferFnRuleElement(transfer.function, convertVariables(transfer.arguments), convertVariable(transfer.result))

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

    constPlans.foreach((tup) => {
      val rule : Rule = tup._1
      val plan : Plan = tup._2

      plan.go(program.translator)
    })

    var done = false

    while (!done) {
      done = true
      // swap inputLatMaps with outputLatMaps
      program.latMapGroups.foreach(_.setInput())
      for (latmapGroup : LatMapGroup <- program.latMapGroups if latmapGroup.inputLatMap.numFacts > 0){
        regPlans.getOrElse(latmapGroup, Seq()).foreach {case (rule, plan) =>
          done = false
          plan.go(program.translator)
        }
      }
    }
  }
}
