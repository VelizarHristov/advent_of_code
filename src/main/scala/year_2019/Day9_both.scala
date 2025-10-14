package year_2019

@main
def day9(): Unit =
  val program = IntcodeComputer.fromInputs("9")
  val res1 = program.clone.run(Seq(1)).head
  val res2 = program.clone.run(Seq(2)).head
  println(res1.toString + " <- part 1")
  println(res2.toString + " <- part 2")
