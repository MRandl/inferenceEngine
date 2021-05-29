package lib.engines

import lib.Proposition
import lib.Rule
import lib.Unifier

object ForwardsChainingEngine {
  def runWithoutTarget(facts : List[Proposition], rules : Set[Rule]) : Set[Proposition] =
    def runIterative(queue : List[Proposition], acc : Set[Proposition]) : Set[Proposition] = 
      queue match {
        case Nil => acc
        case fact :: tail =>
          if acc.contains(fact) then 
            runIterative(tail, acc)
          else
            val validConclusions = {
              for {
                rule        <- rules
                (cond, env) <- rule.dependsOn(fact)
                envs        <- Seq(rule.satisifiedBy(acc + fact, env))
              } yield {
                envs.map(x => Unifier.substitute(rule.conclusion, x))
              }
            }.flatten
            
            runIterative(validConclusions ++: tail, acc ++ validConclusions + fact)
            
      }

    runIterative(facts, Set())
}
