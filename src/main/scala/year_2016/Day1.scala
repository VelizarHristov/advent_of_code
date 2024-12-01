package year_2016

import io.Source
import year_2022.Helpers._

@main
def day1(): Unit = {
  val dirs = Seq((0, 1), (1, 0), (0, -1), (-1, 0))
  var dirIdx = 0
  def dir = dirs(wrapMod(dirIdx, 4))
  var pos = (0, 0)
  for (next <- Source.fromFile("resources/2016/1").getLines.next.split(", ")) {
    dirIdx += (if (next.head == 'R') 1 else -1)
    pos += dir * next.tail.toInt
  }
  val (x, y) = pos
  val res = x.abs + y.abs
  println(res)
}
