package year_2021

import scala.io.Source

@main
def day2_2(): Unit = {
  var horizontalPosition = 0
  var depth = 0
  var aim = 0
  for (line <- Source.fromFile("resources/2").getLines) {
    val Array(direction, amountStr) = line.split(" ")
    val amount = amountStr.toInt
    direction match {
      case "forward" =>
        horizontalPosition += amount
        depth += aim * amount
      case "down" => aim += amount
      case "up" => aim -= amount
    }
  }
  println(horizontalPosition * depth)
}
