package year_2025

import io.Source

@main
def day3_2(): Unit =
  val input = Source.fromFile("resources/2025/3").getLines.map(_.map(_.asDigit))
  val res = input.map(line => {
    var curLine = line
    (11 to 0 by -1).map(remaining => {
      val nextDigit = curLine.dropRight(remaining).max
      curLine = curLine.dropWhile(_ != nextDigit).tail
      nextDigit * Math.pow(10, remaining)
    }).sum
  }).sum
  println(res.toLong)
