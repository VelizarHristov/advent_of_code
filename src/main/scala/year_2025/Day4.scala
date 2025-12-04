package year_2025

import helpers.Helpers.safeGet

import io.Source

@main
def day4(): Unit =
  val grid = Source.fromFile("resources/2025/4").getLines.map(_.toCharArray()).toArray
  val accessible = for
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
  yield true
  println(accessible.size)
