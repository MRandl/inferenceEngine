import lib.*

@main def hello: Unit = 
  val rule = Rule(List(Constant("flies")), Constant("bird"))
  val constant = Constant("flies")
  println(
    lib.engines.ForwardsChainingEngine.runWithoutTarget(List(constant), Set(rule))
  )
  System.out.flush