package year_2025

import io.Source

@main
def day6(): Unit =
  val input = Source.fromFile("resources/2025/6").getLines.map(_.split("\\s+")).toArray.transpose
  val res = input.map(line => {
    val nums = line.init.map(_.toLong)
    line.last.head match
      case '*' => nums.product
      case '+' => nums.sum
  }).sum

  println(res)
