package year_2024

import io.Source
import collection.mutable

import helpers.Helpers._

@main
def day16_2(): Unit = {
  val dirs = Array((1, 0), (0, 1), (-1, 0), (0, -1))
  val grid = Source.fromFile("resources/2024/16").getLines.toArray.map(_.toArray)
  val startPos@(startX, startY) = (for
    y <- grid.indices.view; x <- grid(y).indices if grid(y)(x) == 'S'
  yield (x, y)).head
  grid(startY)(startX) = '.'
  val startDirIdx = 0
  val reaches = mutable.Map(
    (startPos, startDirIdx) -> Set.empty[((Int, Int), Int)]
  ).withDefaultValue(Set.empty)
  val visited = mutable.Map((startPos, startDirIdx) -> 0).withDefaultValue(Int.MaxValue)
  val states = mutable.PriorityQueue(
    ((startPos, startDirIdx), 0)
  )((s1, s2) => s2._2.compare(s1._2))
  var minCost = Int.MaxValue
  while (states.head._2 <= minCost) {
    val (state@(pos@(x, y), dirIdx), cost) = states.dequeue()
    if (grid(y)(x) == 'E') {
      minCost = minCost.min(cost)
    } else if (cost < minCost) {
      val rotateStates = for (rotation <- List(-1, 1))
        yield ((pos, wrapMod(dirIdx + rotation, 4)), cost + 1000)
      val moveStateOpt = {
        val nextPos@(nextX, nextY) = pos + dirs(dirIdx)
        val nextTile = safeGet(grid, nextY, nextX)
        if (nextTile == Some('.') || nextTile == Some('E'))
          Some((nextPos, dirIdx), cost + 1)
        else
          None
      }
      val nextStates = (rotateStates ++ moveStateOpt)
      for ((nextState, nextCost) <- nextStates) {
        if (visited(nextState) > nextCost) {
          visited(nextState) = nextCost
          reaches(nextState) = Set(state)
          states += ((nextState, nextCost))
        } else if (visited(nextState) == nextCost) {
          reaches(nextState) += state
        }
      }
    }
  }
  val endPos = (for
    y <- grid.indices.view; x <- grid(y).indices if grid(y)(x) == 'E'
  yield (x, y)).head
  val optimalPathPositions = mutable.Set(endPos)
  var optimalStates = (0 to 3).map(dirIdx => (endPos, dirIdx))
    .filter(visited(_) == minCost)
  while (optimalStates.nonEmpty) {
    optimalStates = optimalStates.flatMap(reaches)
    optimalPathPositions ++= optimalStates.map(_._1)
  }

  println(optimalPathPositions.size)
}
