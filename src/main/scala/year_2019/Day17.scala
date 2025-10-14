package year_2019

@main
def day17(): Unit =
  val program = IntcodeComputer.fromInputs("17")
  val output = program.run(List()).map(_.toChar).mkString
  val grid = output.split('\n')
  val products = for
    i <- grid.indices.tail.init
    j <- grid(i).indices.tail.init
    if grid(i)(j) == '#'
    if Seq((1, 0), (-1, 0), (0, 1), (0, -1)).forall:
      (di, dj) => grid(i + di)(j + dj) == '#'
  yield i * j

  val res = products.sum
  println(res)
