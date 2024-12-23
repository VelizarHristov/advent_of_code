package year_2024

import io.Source
import collection.mutable

import helpers.Helpers._

@main
def day18(): Unit = {
  val dirs = List((0, 1), (0, -1), (1, 0), (-1, 0))
  val filename = "resources/2024/18"
  val gridSize = if (filename.contains("test")) 7 else 71
  val inputSize = if (filename.contains("test")) 12 else 1024
  val occupied = Source.fromFile(filename).getLines.take(inputSize).map(line => {
    val Array(x, y) = line.split(',').map(_.toInt)
    (x, y)
  }).toSet
  val visited = mutable.Set((0, 0))
  def cannotVisit(x: Int, y: Int): Boolean =
    x < 0 || x >= gridSize ||
      y < 0 || y >= gridSize ||
      occupied.contains((x, y)) ||
      visited.contains((x, y))
  val states = mutable.PriorityQueue(((0, 0), 0))((s1, s2) => s2._2.compare(s1._2))
  var found: Option[Int] = None
  while (found == None) {
    val (nextPos, moves) = states.dequeue()
    if (nextPos == (gridSize - 1, gridSize - 1)) {
      found = Some(moves)
    } else {
      val nextStates = dirs.map(dir => (nextPos + dir, moves + 1)).filter{
        case ((x, y), _) => !cannotVisit(x, y)
      }
      states ++= nextStates
      visited ++= nextStates.map(_._1)
    }
  }

  println(found.get)
}
