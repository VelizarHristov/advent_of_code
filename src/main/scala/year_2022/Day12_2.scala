package year_2022

import year_2022.Helpers.safeGet

import io.Source
import scala.collection.mutable

@main
def day12_2(): Unit = {
  val rawGrid = Source.fromFile("resources/2022/12_test").getLines.toArray.map(_.toCharArray)
  for (i <- rawGrid.indices; j <- rawGrid(0).indices; if rawGrid(i)(j) == 'S')
    rawGrid(i)(j) = 'a'
  val startPositions = for i <- rawGrid.indices; j <- rawGrid(0).indices; if rawGrid(i)(j) == 'a' yield (i, j)
  val endPos = (for i <- rawGrid.indices; j <- rawGrid(0).indices; if rawGrid(i)(j) == 'E' yield (i, j)).head
  rawGrid(endPos._1)(endPos._2) = 'z'
  val grid = rawGrid.toIndexedSeq.map(_.toIndexedSeq)

  val visited = startPositions.to(mutable.Set)
  val currentStates = startPositions.to(mutable.ListBuffer)
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
