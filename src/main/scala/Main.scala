import lib.*

@main def hello: Unit = 
  val flies = Constant("flies")
  val swims = Constant("swims")
  val duck  = Constant("duck")
  val fish  = Constant("fish")
  val alien = Constant("alien")

  val duckFlies = Rule(Set(duck), flies)
  val duckSwims = Rule(Set(duck), swims)
  val fishSwims = Rule(Set(fish), swims)
  
  println(
    lib.engines.ForwardsChainingEngine.runWithoutTarget(List(duck), Set(duckFlies, fishSwims, duckSwims))
  )
  System.out.flush