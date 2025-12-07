package year_2025

import helpers.Helpers._

import io.Source

@main
def day7(): Unit =
  val grid = Source.fromFile("resources/2025/7").getLines.toArray
  val (startY, startX) = (for
    y <- grid.indices
    x <- grid(y).indices
    if grid(y)(x) == 'S'
  yield (y, x)).head

  var beams = Vector(startX)
  val totalSplits = ((startY + 1) until grid.size).map(y => {
    val splitCount = beams.count(x => grid(y)(x) == '^')
    beams = beams.flatMap(x => {
      if grid(y)(x) == '^' then
        Seq(x - 1, x + 1)
      else
        Seq(x)
    }).distinct
    splitCount
  }).sum
  println(totalSplits)
