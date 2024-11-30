package year_2022

import io.Source
import collection.mutable.{ArrayBuffer, Set => MutableSet}

@main
def day24(): Unit = {
  val dirMap = Map(
    '^' -> (0, -1),
    'v' -> (0, 1),
    '<' -> (-1, 0),
    '>' -> (1, 0))
  val input = Source.fromFile("resources/2022/24").getLines.zipWithIndex.toVector
  val maxY = input.size - 2
  val maxX = input.head._1.size - 2
  val startBlizzards = (for {
    (line, y) <- input
    (char, x) <- line.zipWithIndex
    if dirMap.contains(char)
  } yield {
    (x, y) -> dirMap(char)
  })
  val blizzardsAt = ArrayBuffer(startBlizzards)
  val gridAt = ArrayBuffer(startBlizzards.map(_._1).toSet)
  def getGrid(timestep: Int): Set[(Int, Int)] = {
    if (gridAt.size <= timestep) {
      val blizzards = blizzardsAt(timestep - 1).map{ case ((x, y), (dx, dy)) => {
        def wrap(i: Int, maxI: Int) = {
          if (i == 0) {
            maxI
          } else if (i == maxI + 1) {
            1
          } else {
            i
          }
        }
        val nextX = wrap(x + dx, maxX)
        val nextY = wrap(y + dy, maxY)
        ((nextX, nextY), (dx, dy))
      }}
      blizzardsAt += blizzards
      gridAt += blizzards.map(_._1).toSet
    }
    gridAt(timestep)
  }

  case class State(x: Int, y: Int, timestep: Int)
  val states = ArrayBuffer(State(1, 0, 0))
  val visited = MutableSet(states.head)
  var finalRes: Option[Int] = None
  while (finalRes.isEmpty) {
    val State(prevX, prevY, prevTimestep) = states.remove(0)
    val timestep = prevTimestep + 1
    val grid = getGrid(timestep)
    val next = (dirMap.values.toSeq :+ (0, 0)).map(
      (dx, dy) => State(prevX + dx, prevY + dy, timestep))
    if (next.exists{ case State(x, y, _) => (x, y) == (maxX, maxY + 1) })
      finalRes = Some(timestep)
    val nextStates = next.filter{ case st@State(x, y, _) =>
      x >= 1 && y >= 1 &&
      x <= maxX && y <= maxY &&
      !grid((x, y)) &&
      !visited(st)
    }
    states ++= nextStates
    visited ++= nextStates
  }

  println(finalRes.get)
}
