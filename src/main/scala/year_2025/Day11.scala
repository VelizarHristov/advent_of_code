package year_2025

import io.Source

@main
def day11(): Unit =
  val input = Source.fromFile("resources/2025/11").getLines.toArray.map(line => {
    val Array(from, to) = line.split(": ")
    val dsts = to.split(' ')
    (from, dsts)
  }).toMap

  def countPaths(next: String): Int = next match
    case "out" => 1
    case _     => input(next).map(countPaths).sum
  val res = countPaths("you")
  println(res)
