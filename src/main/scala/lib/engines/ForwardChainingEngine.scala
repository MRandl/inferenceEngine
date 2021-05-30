package lib.engines

import lib.Proposition
import lib.Rule
import lib.Unifier

import scala.collection.immutable.Queue

object ForwardsChainingEngine {

  def runWithoutTarget(facts : Set[Proposition], rules : Set[Rule]) : Set[Proposition] =
    runIterative(facts.toList, Set(), rules)

  @scala.annotation.tailrec
  private def runIterative(queue : List[Proposition], acc : Set[Proposition], rules : Set[Rule]) : Set[Proposition] = 
    queue match {
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
              val en = rule.satisifiedBy(newAcc, env).map(x => Unifier.substitute(rule.conclusion, x))
              en
            }
          }.flatten
          runIterative(tail ++ validConclusions, newAcc, rules)
          
      case Nil => 
        acc
          
    }

}
