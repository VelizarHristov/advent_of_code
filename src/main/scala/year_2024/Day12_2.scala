package year_2024

import io.Source
import collection.mutable

import helpers.Helpers.safeGet

@main
def day12_2(): Unit = {
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
    val pointsInArea = mutable.Set((startY, startX))
    val remainingPoints = mutable.Set((startY, startX))
    var total = 0
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
          pointsInArea += ((newY, newX))
        }
        remainingPoints -= ((y, x))
        total += 1
      }
    }
    val numSides = (for ((dy, dx) <- dirs) yield {
      val points = if (dx == 0) pointsInArea else pointsInArea.map(_.swap)
      val d0 = if (dy.max(dx) == 1) -1 else 1
      var total = 0
      // grid is square in all datasets
      for (y <- 0 until grid.size) {
        var connected = false
        for (x <- 0 until grid.size) {
          val atPlant = points.contains((y, x))
          val atBorder = !points.contains((y + d0, x))
          if (atPlant && atBorder && !connected)
            total += 1
          connected = atPlant && atBorder
        }
      }
      total
    }).sum
    (total, numSides)
  }
  val res = regionSizes.map((a, b) => a * b).sum

  println(res)
}
