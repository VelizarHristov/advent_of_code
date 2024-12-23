package year_2024

import io.Source
import collection.mutable

import helpers.Helpers._

@main
def day20_2(): Unit = {
  val dirs = List((0, 1), (0, -1), (1, 0), (-1, 0))
  val grid = Source.fromFile("resources/2024/20").getLines.map(_.toCharArray).toArray
  val (startX, startY) = (for (
    y <- grid.indices.view; x <- grid(y).indices if grid(y)(x) == 'S'
  ) yield (x, y)).head
  val end@(endX, endY) = (for (
    y <- grid.indices.view; x <- grid(y).indices if grid(y)(x) == 'E'
  ) yield (x, y)).head
  grid(startY)(startX) = '.'
  grid(endY)(endX) = '.'
  val distances = mutable.Map(end -> 0)
  val positions = mutable.ListBuffer(end)
  while (positions.nonEmpty) {
    val pos = positions.remove(0)
    for {
      dir <- dirs
      nextPos@(x, y) = pos + dir
      if !distances.contains(nextPos)
      if grid(y)(x) != '#'
    } {
      positions += nextPos
      distances(nextPos) = distances(pos) + 1
    }
  }
  // Note: in our data, there is only one possible path from any two points
  // Were in not the case, we would have needed to do more work
  val moves = for (dx <- -20 to 20; dy <- (-20 + dx.abs) to (20 - dx.abs))
    yield (dx, dy)
  val res = distances.map((pos, curDistance) => {
    (for {
      move@(dx, dy) <- moves
      newPos@(x, y) = pos + move
      if safeGet(grid, y, x) == Some('.')
      newDistance = distances(newPos) + dx.abs + dy.abs
      if curDistance - newDistance >= 100
    } yield 1).sum
  }).sum

  println(res)
}