package year_2025

import io.Source

@main
def day2(): Unit =
  val input = Source.fromFile("resources/2025/2").getLines.next.split(',').map: rangeStr =>
    val Array(start, end) = rangeStr.split('-').map(_.toLong)
    start to end
  val res = input.map(_.filter(i => {
    val str = i.toString
    val prefix = str.take(str.length / 2)
    str == prefix * 2
  }).sum).sum
  println(res)
