package year_2019

@main
def day13(): Unit =
  val programOutput = IntcodeComputer.fromInputs("13").run(Seq())
  val res = programOutput.grouped(3).count(_(2) == 2)
  println(res)
