package year_2025

import io.Source

@main
def day3(): Unit =
  val input = Source.fromFile("resources/2025/3").getLines.map(_.map(_.asDigit))
  val res = input.map(line => {
    val firstDigit = line.init.max
    val firstIdx = line.indexOf(firstDigit)
    val secondDigit = line.drop(firstIdx + 1).max
    firstDigit * 10 + secondDigit
  }).sum
  println(res)
