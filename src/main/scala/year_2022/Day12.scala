package year_2022

import helpers.Helpers.safeGet

import io.Source
import scala.collection.mutable

@main
def day12(): Unit = {
  val rawGrid = Source.fromFile("resources/2022/12").getLines.toArray.map(_.toCharArray)
  val startPos = (for i <- rawGrid.indices; j <- rawGrid(0).indices; if rawGrid(i)(j) == 'S' yield (i, j)).head
  val endPos = (for i <- rawGrid.indices; j <- rawGrid(0).indices; if rawGrid(i)(j) == 'E' yield (i, j)).head
  rawGrid(startPos._1)(startPos._2) = 'a'
  rawGrid(endPos._1)(endPos._2) = 'z'
  val grid = rawGrid.toIndexedSeq.map(_.toIndexedSeq)

  val visited = mutable.Set(startPos)
  val currentStates = mutable.ListBuffer(startPos)
  var steps = 0
  while (!currentStates.contains(endPos)) {
    val nextStates = for ((y, x) <- currentStates) yield {
      for ((dy, dx) <- Seq((1, 0), (-1, 0), (0, 1), (0, -1))) yield {
        safeGet(grid, y + dy, x + dx) match {
          case Some(c) if c <= grid(y)(x) + 1 => Seq((y + dy, x + dx))
          case _ => Seq()
        }
      }
    }
    currentStates.clear()
    for (next <- nextStates.flatten.flatten) {
      if (!visited.contains(next)) {
        visited += next
        currentStates += next
      }
    }
    steps += 1
  }

  println(steps)
}
