package lib

object Unifier {

  def substitute(pattern : Proposition, env : Environment) : Proposition = 
    pattern match {
      case va : Variable => 
        env.get(va)
          .map(substitute(_, env))
          .getOrElse(va)
      case ct : Constant =>
        ct
      case Composite(name, on) => 
        Composite(name, on.map(substitute(_, env)))
    }
  
  private def unify(first : Proposition, second : Proposition) : Option[Environment] =
    def atomic_unify(first : Atom, second : Proposition) : Option[Environment] = 
      if(first == second)
        Some(Map())
      else
        first match {
          case va : Variable => 
            Option.when(!second.vars.contains(va.name))(Map(va -> second))
          case co : Constant =>
            second.toVariableOption.map(va => Map(va -> first))
        }

    first match {
        case at : Atom => atomic_unify(at, second)
        case comp : Composite =>
          second match {
            case at : Atom => atomic_unify(at, first)
            case seccomp : Composite => 
              if(comp.on.isEmpty && seccomp.on.isEmpty && comp.name == seccomp.name)
                Some(Map())
              else if (comp.on.isEmpty || seccomp.on.isEmpty || comp.name != seccomp.name)
                None
              else
                unify(comp.on.head, seccomp.on.head).flatMap { env => 
                  unify(
                    substitute(Composite(comp.name, comp.on.tail), env), 
                    substitute(Composite(seccomp.name, seccomp.on.tail), env)
                  ).map(env2 => env ++ env2)
                }
          }
      
    }

  def patternMatch(first : Proposition, second : Proposition, envOpt : Option[Environment]) : Option[Environment] =
    envOpt match {
      case None => unify(first, second)
      case Some(env) => unify(substitute(first, env), substitute(second, env)).map(_ ++ env)
    }

}
