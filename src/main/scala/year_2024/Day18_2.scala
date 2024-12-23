package year_2024

import io.Source
import collection.mutable

import helpers.Helpers._

@main
def day18_2(): Unit = {
  val dirs = List((0, 1), (0, -1), (1, 0), (-1, 0))
  val filename = "resources/2024/18"
  val allWalls = Source.fromFile(filename).getLines.map(line => {
    val Array(x, y) = line.split(',').map(_.toInt)
    (x, y)
  }).toVector
  val gridSize = if (filename.contains("test")) 7 else 71

  var toTake = if (filename.contains("test")) 12 else 1024
  val walls = allWalls.take(toTake).to(mutable.Set)
  var found = true
  while (found) {
    toTake += 1
    walls += allWalls(toTake)
    val visited = mutable.Set((0, 0))
    def cannotVisit(x: Int, y: Int): Boolean =
      x < 0 || x >= gridSize ||
        y < 0 || y >= gridSize ||
        walls.contains((x, y)) ||
        visited.contains((x, y))
    val states = mutable.ListBuffer((0, 0))
    found = false
    while (!found && states.nonEmpty) {
      val nextPos = states.remove(0)
      val nextStates = dirs.map(_ + nextPos).filterNot(cannotVisit)
      states ++= nextStates
      visited ++= nextStates
      found = nextPos == (gridSize - 1, gridSize - 1)
    }
  }

  println(allWalls(toTake))
}
