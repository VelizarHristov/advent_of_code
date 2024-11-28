package year_2021

import scala.collection.mutable
import scala.io.Source

@main
def day15_2(): Unit = {
  val input = Source.fromFile("resources/15").getLines.toVector.map(_.split("").map(_.toInt))
  def mapNum(n: Int, k: Int) = if (n + k >= 10) n + k - 9 else n + k
  val grid = for {
    i <- 0 to 4
    line <- input
  } yield for {
    j <- 0 to 4
    num <- line
  } yield mapNum(num, i + j)
  val visited = mutable.Map(((0, 0), 0)).withDefaultValue(Int.MaxValue)
  case class State(pos: (Int, Int), cost: Int)
  val finalPos = (grid.size - 1, grid.head.size - 1)
  val queue = mutable.PriorityQueue[State](State((0, 0), 0))((s1, s2) => s2.cost.compare(s1.cost))
  while (queue.nonEmpty) {
    val State((y, x), cost) = queue.dequeue()
    queue ++= (for {
      (dy, dx) <- Seq((1, 0), (-1, 0), (0, 1), (0, -1))
      nextPos = (y + dy, x + dx)
      nextCost <- safeGet(grid, nextPos._1, nextPos._2).map(_ + cost)
      if nextCost < visited(nextPos).min(visited(finalPos))
    } yield {
      visited(nextPos) = nextCost
      State(nextPos, nextCost)
    })
  }
  println(visited(finalPos))
}
