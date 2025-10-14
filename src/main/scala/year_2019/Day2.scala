package year_2019

import io.Source

@main
def day2(): Unit =
  val filename = "resources/2019/2"
  val prog = Source.fromFile(filename).mkString.split(',').map(_.toInt)
  if (!filename.endsWith("test")) then
    prog(1) = 12
    prog(2) = 2
  var pos = 0
  while (prog(pos) != 99)
    val n1 = prog(prog(pos + 1))
    val n2 = prog(prog(pos + 2))
    val dst = prog(pos + 3)
    prog(dst) = prog(pos) match
      case 1 => n1 + n2
      case 2 => n1 * n2
    pos += 4

  println(prog(0))
