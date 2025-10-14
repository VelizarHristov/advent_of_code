package year_2019

import io.Source

@main
def day2_2(): Unit =
  def runProgram(prog: Array[Int]) =
    var pos = 0
    while (prog(pos) != 99)
      val n1 = prog(prog(pos + 1))
      val n2 = prog(prog(pos + 2))
      prog(prog(pos + 3)) = prog(pos) match
        case 1 => n1 + n2
        case 2 => n1 * n2
      pos += 4

  val filename = "resources/2019/2"
  val originalProg = Source.fromFile(filename).mkString.split(',').map(_.toInt)
  val target = 19690720
  for (input1 <- 0 to 100; input2 <- 0 to 100)
    val prog = originalProg.clone()
    prog(1) = input1
    prog(2) = input2
    runProgram(prog)
    if (prog(0) == target)
      println(100 * input1 + input2)
      System.exit(0)
  println("Error: ended prematurely")
