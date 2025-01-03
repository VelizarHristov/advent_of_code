package year_2019

import io.Source

@main
def day2_2(): Unit = {
  val filename = "resources/2019/2"
  val originalProg = Source.fromFile(filename).getLines.next.split(',').map(_.toInt)
  val target = 19690720
  for (input1 <- originalProg.indices; input2 <- originalProg.indices) {
    val prog = originalProg.clone()
    prog(1) = input1
    prog(2) = input2
    var pos = 0
    while (prog(pos) != 99) {
      val n1 = prog(prog(pos + 1))
      val n2 = prog(prog(pos + 2))
      prog(pos + 3) = prog(pos) match {
        case 1 => n1 + n2
        case 2 => n1 * n2
      }
      pos += 4
    }
    if (prog(0) == target) {
      println(100 * input1 + input2)
      System.exit(0)
    }
  }
}
