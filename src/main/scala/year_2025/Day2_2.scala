package year_2025

import io.Source

@main
def day2_2(): Unit =
  val input = Source.fromFile("resources/2025/2").getLines.next.split(',').map: rangeStr =>
    val Array(start, end) = rangeStr.split('-').map(_.toLong)
    start to end
  val res = input.map(_.filter(i => {
    val str = i.toString
    val prefixes = (1 to str.length / 2).map(str.take)
    prefixes.exists: prefix =>
      val completed = prefix * (str.length / prefix.length)
      str == completed
  }).sum).sum
  println(res)
