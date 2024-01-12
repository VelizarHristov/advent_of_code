import collection.mutable
import io.Source

@main
def day23_2(): Unit = {
  case class State(items: Map[(Int, Int), Char], cost: Int)
  val correctRooms = Map('A' -> 2, 'B' -> 4, 'C' -> 6, 'D' -> 8)
  val movementCosts = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val roomCoords = correctRooms.values.toSet
  val corridorCoords = (0 to 10)
    .filterNot(roomCoords.contains)
    .map(x => (x, 0))
  val nextStep = (for {
    from <- 0 until 10
  } yield {
    corridorCoords.map(_._1).filter(_ > from).min
  }).toArray

  var startState = State(Map(), 0)
  val input = Source.fromFile("resources/23").getLines.toVector
  val startGrid = (input.take(3)
    :+ "  #D#C#B#A#"
    :+ "  #D#B#A#C#")
    ++ input.drop(3)
  for {
    i <- startGrid.indices
    j <- 0 until startGrid(i).length
    if !"#. ".contains(startGrid(i)(j))
  } {
    startState = State(startState.items + (((j - 1, i - 1), startGrid(i)(j))), 0)
  }

  val states = mutable.Stack(startState)
  val visited = mutable.Map[Map[(Int, Int), Char], Int]().withDefaultValue(Int.MaxValue)
  var bestSolution = Int.MaxValue
  while (states.nonEmpty) {
    val state = states.pop()
    val costUpperBound = visited(state.items) min bestSolution
    if (state.cost < costUpperBound) {
      visited(state.items) = state.cost
      val occupied = state.items.keySet
      val memo = mutable.Map[(Int, Int), Boolean]()
      def canGo(from: Int, to: Int): Boolean = {
        if (from > to)
          return canGo(to, from)
        else if (from + 1 >= to)
          return true
        lazy val res = {
          val next = nextStep(from)
          next == to || (!occupied((next, 0)) && canGo(next, to))
        }
        memo.getOrElseUpdate((from, to), res)
      }
      val nextStates = for {
        ((itemX, itemY), item) <- state.items
        atRoom = itemY >= 1
        finalX = correctRooms(item)
        if !atRoom || !occupied((itemX, itemY - 1))
        if itemX != finalX || state.items.get(itemX, itemY + 1).exists(_ != item)
        (x, y) <- {
          if (atRoom) {
            corridorCoords.view.filterNot(occupied.contains).filter(c => canGo(itemX, c._1))
          } else {
            val spots = (1 to 4).map(y => (y, state.items.get(finalX, y)))
            val dest = if (spots.forall(_._2.forall(_ == item)))
              spots.findLast(_._2.isEmpty).map(i => (finalX, i._1)).toSeq
            else
              Seq()
            dest.filter(xy => canGo(itemX, xy._1))
          }
        }
      } yield {
        val nextItems = state.items.removed((itemX, itemY)) + (((x, y), item))
        val cost = ((itemX - x).abs + (itemY - y).abs) * movementCosts(item)
        State(nextItems, state.cost + cost)
      }
      states.pushAll(nextStates)
      if (nextStates.isEmpty && state.items.forall(_._1._2 > 0))
        bestSolution = state.cost
    }
  }
  println(bestSolution)
}
