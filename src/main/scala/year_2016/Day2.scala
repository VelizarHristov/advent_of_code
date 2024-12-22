package year_2016

import io.Source
import helpers.Helpers._

@main
def day2(): Unit = {
  val dirs = Map(
    'U' -> (0, -1),
    'D' -> (0, 1),
    'L' -> (-1, 0),
    'R' -> (1, 0))
  val keypad = Seq(
    "123",
    "456",
    "789").map(_.toCharArray.toSeq)
  var pos = (1, 1)
  val codes = for (next <- Source.fromFile("resources/2016/2").getLines.toSeq) yield {
    for (c <- next) {
      val nextPos@(x, y) = pos + dirs(c)
      if (safeGet(keypad, y, x).isDefined)
        pos = nextPos
    }
    val (x, y) = pos
    keypad(y)(x)
  }
  println(codes.mkString)
}
