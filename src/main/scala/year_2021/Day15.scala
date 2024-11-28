package year_2021

import scala.io.Source
import collection.mutable
import collection.mutable.ArraySeq

@main
def day15(): Unit = {
  val grid = Source.fromFile("resources/2021/15").getLines.to(ArraySeq).map(_.split("").map(_.toInt).to(ArraySeq))
  val visited = mutable.Map(((0, 0), 0)).withDefaultValue(Int.MaxValue)
  case class State(pos: (Int, Int), cost: Int)
  var states = Seq(State((0, 0), 0))
  val finalPos = (grid.size - 1, grid.head.size - 1)
  while (states.nonEmpty) {
    states = for {
      State((y, x), cost) <- states
      (dy, dx) <- Seq((1, 0), (-1, 0), (0, 1), (0, -1))
      nextPos = (y + dy, x + dx)
      nextCost <- safeGet(grid, nextPos._1, nextPos._2).map(_ + cost)
      if nextCost < visited(nextPos).min(visited(finalPos))
    } yield {
      visited(nextPos) = nextCost
      State(nextPos, nextCost)
    }
  }
  println(visited(finalPos))
}
