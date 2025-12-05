package year_2025

import io.Source

@main
def day5_2_Simpler(): Unit =
  val input = Source.fromFile("resources/2025/5").getLines
  val ranges = input.takeWhile(_ != "").map(line => {
    val Array(a, b) = line.split('-').map(_.toLong)
    (a, b)
  }).toVector.sortBy(_._1)

  var sum = 0L
  var last = 0L
  ranges.foreach: (a, b) =>
    if a <= last then
      sum += 0L max (b - last)
    else
      sum += b - a + 1
    last = b max last
  println(sum)
