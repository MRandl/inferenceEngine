import lib.*

@main def hello: Unit = 

  val knowsIsTransitive  = Rule(Set(knows("x", "y"), knows("y", "z")), knows("x", "z"))
  val knowsIsCommutative = Rule(Set(knows("x", "y")), knows("y", "x"))

  val duckKnowsFish  = Composite("knows", List(Constant("duck"), Constant("fish")))
  val fishKnowsAlien = Composite("knows", List(Constant("fish"), Constant("alien")))
  val lamaKnowsAlien = Composite("knows", List(Constant("lama"), Constant("alien")))

  println(
    lib.engines.ForwardsChainingEngine.runWithoutTarget(
      Set(duckKnowsFish, fishKnowsAlien, lamaKnowsAlien), 
      Set(knowsIsTransitive, knowsIsCommutative)
    ).map(_.toString).toList.sorted.mkString("\n")
  )
  System.out.flush

def knows(x : String, y : String) = 
  Composite("knows", List(Variable(x), Variable(y)))

def being(x : String) = 
  Composite("being", List(Variable(x)))
