package lib

case class Rule(conditions : Set[Proposition], conclusion : Proposition) {

  def dependsOn(fact : Proposition) : Set[Environment] = 
    conditions.map(cond => Unifier.patternMatch(fact, cond, None)).filter(_.isDefined).map(_.get)
  
  def satisifiedBy(facts : Set[Proposition], env : Environment) =
    conditions.foldLeft(Set(env)){(envs, cond) => 
      facts.flatMap(fact => 
        envs.map(env => 
          Unifier.patternMatch(fact, cond, Some(env))
        ).filter(_.isDefined).map(_.get)
      )
    }
}

