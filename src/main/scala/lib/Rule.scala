package lib

case class Rule(conditions : List[Proposition], conclusion : Proposition) {

  def dependsOn(fact : Proposition) : List[(Proposition, Environment)] = 
    conditions.map(cond => (cond, Unifier.patternMatch(fact, cond, None))).filter(x => x(1).isDefined).map(x => (x(0), x(1).get))
  
  def satisifiedBy(facts : Set[Proposition], env : Environment) =
    conditions.foldLeft(Set(env)){(envs, cond) => 
      facts.flatMap(fact => 
        envs.map(env => 
          Unifier.patternMatch(fact, cond, Some(env))
        ).filter(_.isDefined).map(_.get)
      )
    }
}

