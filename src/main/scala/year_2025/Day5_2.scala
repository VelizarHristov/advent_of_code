package year_2025

import io.Source
import collection.mutable.ArrayBuffer

@main
def day5_2(): Unit =
  val input = Source.fromFile("resources/2025/5").getLines
  val ranges = input.takeWhile(_ != "").map(line => {
    val Array(a, b) = line.split('-').map(_.toLong)
    (a, b)
  }).to(ArrayBuffer).sortBy(_._1)

  var i = 0
  while (i < ranges.size - 1)
    val (a, b) = ranges(i)
    val (c, d) = ranges(i + 1)
    if c <= b then
      ranges(i) = (a, b max d)
      ranges.remove(i + 1)
    else
      i += 1

  val res = ranges.map((a, b) => b - a + 1).sum
  println(res)
