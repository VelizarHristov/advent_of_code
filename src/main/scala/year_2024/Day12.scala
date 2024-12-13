package year_2024

import io.Source
import collection.mutable

import year_2022.Helpers.safeGet

@main
def day12(): Unit = {
  val dirs = Vector(
    (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0))
  val grid = Source.fromFile("resources/2024/12").getLines.map(_.toCharArray).toArray
  val visited = mutable.Set[(Int, Int)]()
  val regionSizes = for {
    startY <- grid.indices
    startX <- grid(startY).indices
    if !visited((startY, startX))
  } yield {
    val plantType = grid(startY)(startX)
    val remainingPoints = mutable.Set((startY, startX))
    var total = 0
    var totalPerimeter = 0
    while (remainingPoints.nonEmpty) {
      remainingPoints.foreach { (y, x) =>
        visited += ((y, x))
        for {
          (dy, dx) <- dirs
          newY = y + dy
          newX = x + dx
          if !visited((newY, newX))
          if safeGet(grid, newY, newX) == Some(plantType)
        } {
          remainingPoints += ((newY, newX))
        }
        remainingPoints -= ((y, x))
        total += 1
        totalPerimeter += dirs.count((dy, dx) =>
          safeGet(grid, y + dy, x + dx) != Some(plantType))
      }
    }
    (total, totalPerimeter)
  }
  val res = regionSizes.map((a, b) => a * b).sum

  println(res)
}
