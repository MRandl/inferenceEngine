package lib.engines

import lib.Proposition
import lib.Rule
import lib.Unifier

object ForwardsChainingEngine {

  def runWithoutTarget(facts : List[Proposition], rules : Set[Rule]) : Set[Proposition] =
    runIterative(facts, Set(), rules)

  @scala.annotation.tailrec
  private def runIterative(queue : List[Proposition], acc : Set[Proposition], rules : Set[Rule]) : Set[Proposition] = 
    queue match {
      case Nil => acc
      case fact :: tail =>
        if acc.contains(fact) then 
          runIterative(tail, acc, rules)
        else
          val newAcc = acc + fact
          val validConclusions = {
            for {
              rule <- rules
              env  <- rule.dependsOn(fact) //check the current fact is relevant to the current rule
            } yield {
              rule.satisifiedBy(newAcc, env).map(x => Unifier.substitute(rule.conclusion, x))
            }
          }.flatten
          
          runIterative(tail :++ validConclusions, newAcc ++ validConclusions, rules)
          
    }

}
