package year_2025

import io.Source

@main
def day12(): Unit =
  val input = Source.fromFile("resources/2025/12").getLines.drop(30)
  val res = input.count(line => {
    val parts = line.split(": ")
    val Array(length, width) = parts.head.split('x').map(_.toInt)
    val quantities = parts.last.split(' ').map(_.toInt)
    val availableSpace = length * width
    if availableSpace >= quantities.sum * 9 then
      true  // trivial to solve
    else if availableSpace < quantities.sum * 7 then
      false // impossible to solve
    else
      throw new IllegalStateException("not implemented")
  })
  println(res)
