package year_2025

import io.Source

@main
def day5(): Unit =
  val input = Source.fromFile("resources/2025/5").getLines
  val ranges = input.takeWhile(_ != "").toArray.map: line =>
    val Array(a, b) = line.split('-').map(_.toLong)
    (a, b)
  val nums = input.map(_.toLong).toArray

  val res = nums.count: x =>
    ranges.exists: (a, b) =>
      a <= x && x <= b
  println(res)
