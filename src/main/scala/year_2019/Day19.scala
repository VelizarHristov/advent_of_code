package year_2019

@main
def day19(): Unit =
  val code = IntcodeComputer.fromInputs("19").code
  val spots =
    for
      i <- 0 until 50
      j <- 0 until 50
      res = IntcodeComputer(code.clone()).run(Vector(i, j)).head
      if res == 1
    yield (i, j)
  val res = spots.size
  println(res)
