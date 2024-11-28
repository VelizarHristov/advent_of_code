package year_2021

import scala.io.Source

@main
def day2(): Unit = {
  var horizontalPosition = 0
  var depth = 0
  for (line <- Source.fromFile("resources/2021/2").getLines) {
    val Array(direction, amountStr) = line.split(" ")
    val amount = amountStr.toInt
    direction match {
      case "forward" => horizontalPosition += amount
      case "down" => depth += amount
      case "up" => depth -= amount
    }
  }
  println(horizontalPosition * depth)
}
