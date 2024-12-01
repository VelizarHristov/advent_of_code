package year_2016

import io.Source
import year_2022.Helpers._

@main
def day2_2(): Unit = {
  val dirs = Map(
    'U' -> (0, -1),
    'D' -> (0, 1),
    'L' -> (-1, 0),
    'R' -> (1, 0))
  val keypad = Seq(
    "  1  ",
    " 234 ",
    "56789",
    " ABC ",
    "  D  ").map(_.toCharArray.toSeq)
  var pos = (0, 2)
  val codes = for (next <- Source.fromFile("resources/2016/2").getLines.toSeq) yield {
    for (c <- next) {
      val nextPos@(x, y) = pos + dirs(c)
      if (safeGet(keypad, y, x).getOrElse(' ') != ' ')
        pos = nextPos
    }
    val (x, y) = pos
    keypad(y)(x)
  }
  println(codes.mkString)
}
