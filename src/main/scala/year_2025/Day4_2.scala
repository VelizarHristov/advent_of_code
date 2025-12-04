package year_2025

import helpers.Helpers.safeGet

import io.Source

@main
def day4_2(): Unit =
  val grid = Source.fromFile("resources/2025/4").getLines.map(_.toCharArray()).toArray
  val initialRollCount = grid.flatten.count(_ == '@')
  var curRollCount = initialRollCount
  var prevRollCount = 0
  while curRollCount != prevRollCount do
    for
      x <- grid.indices
      y <- grid(x).indices
      if grid(x)(y) == '@'
      adjRolls = for
        dx <- -1 to 1
        dy <- -1 to 1
        if (dx, dy) != (0, 0)
        c <- safeGet(grid, x + dx, y + dy)
        if c == '@'
      yield true
      if adjRolls.size < 4
    do
      grid(x)(y) = '.'
    prevRollCount = curRollCount
    curRollCount = grid.flatten.count(_ == '@')

  println(initialRollCount - curRollCount)
