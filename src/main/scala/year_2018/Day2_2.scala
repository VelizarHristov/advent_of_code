package year_2018

import io.Source

@main
def day2_2(): Unit = {
  val input = Source.fromFile("resources/2018/2").getLines.toVector
  val lineLen = input.head.length
  val firstMatch = (for {
    line1 <- input
    line2 <- input
    if line1 != line2
    common = line1.zip(line2).filter((x, y) => x == y).map(_._1).mkString
    if common.length == (lineLen - 1)
  } yield common).head
  println(firstMatch)
}
