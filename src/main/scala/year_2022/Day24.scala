package year_2022

import io.Source
import collection.mutable.{ArrayBuffer, PriorityQueue, Map => MutableMap}

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

  case class State(x: Int, y: Int, timestep: Int) {
    lazy val heuristic = timestep + maxX + maxY + 1 - x - y
  }
  val states = PriorityQueue(State(1, 0, 0))((s1, s2) => s2.heuristic.compare(s1.heuristic))
  val visited = MutableMap(states.head -> states.head.heuristic).withDefaultValue(Int.MaxValue)
  var finalRes: Option[Int] = None
  while (finalRes.isEmpty) {
    val State(prevX, prevY, prevTimestep) = states.dequeue()
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
      visited(st) > st.heuristic
    }
    for (state <- nextStates) {
      states += state
      visited(state) = state.heuristic
    }
  }

  println(finalRes.get)
}
