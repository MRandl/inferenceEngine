package lib

case class Rule(conditions : List[Proposition], conclusion : Proposition) {

  def dependsOn(fact : Proposition) : List[Environment] = 
    conditions.map(cond => Unifier.patternMatch(fact, cond, None)).filter(_.isDefined).map(_.get)
  
  def satisifiedBy(facts : List[Proposition], env : Environment) =
    conditions.foldLeft(List(env)){(envs, cond) => 
      facts.flatMap(fact => 
        envs.map(env => 
          Unifier.patternMatch(fact, cond, Some(env))
        ).filter(_.isDefined).map(_.get)
      )
    }
}

