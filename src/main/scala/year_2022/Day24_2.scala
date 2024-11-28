package year_2022

import io.Source
import collection.mutable.{ArrayBuffer, PriorityQueue, Map => MutableMap}

@main
def day24_2(): Unit = {
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

  val distFromStartToEnd = maxX - 2 + maxY
  case class State(x: Int, y: Int, timestep: Int, tripsNum: Int) {
    val heuristic = {
      val distToStart = x + y - 1
      val distToEnd = maxX + maxY - x - y + 1
      val distToNextGoal = if (tripsNum % 2 == 0) distToEnd else distToStart
      timestep + distToNextGoal + (2 - tripsNum) * distFromStartToEnd
    }
  }
  val states = PriorityQueue(State(1, 0, 0, 0))((s1, s2) => s2.heuristic.compare(s1.heuristic))
  val visited = MutableMap(states.head -> states.head.heuristic).withDefaultValue(Int.MaxValue)
  while (states.head.tripsNum != 3) {
    val State(prevX, prevY, prevTimestep, prevTripsNum) = states.dequeue()
    val timestep = prevTimestep + 1
    val grid = getGrid(timestep)
    val next = (dirMap.values.toSeq :+ (0, 0)).map{ (dx, dy) =>
      val x = prevX + dx
      val y = prevY + dy
      val atGoal = (prevTripsNum % 2 == 0 && (x, y) == (maxX, maxY + 1)) ||
        (prevTripsNum % 2 == 1 && (x, y) == (1, 0))
      val tripsNum = if (atGoal) prevTripsNum + 1 else prevTripsNum
      State(x, y, timestep, tripsNum)
    }
    def atStartOrEnd(x: Int, y: Int) = (
      (x, y) == (1, 0) ||
      (x, y) == (maxX, maxY + 1))
    def inBounds(x: Int, y: Int) = (
      x >= 1 && y >= 1 &&
      x <= maxX && y <= maxY)
    val nextStates = next.filter{ case st@State(x, y, _, _) =>
      (atStartOrEnd(x, y) || inBounds(x, y)) &&
      !grid((x, y)) &&
      visited(st) > st.heuristic
    }
    for (state <- nextStates) {
      states += state
      visited(state) = state.heuristic
    }
  }

  println(states.head.timestep)
}