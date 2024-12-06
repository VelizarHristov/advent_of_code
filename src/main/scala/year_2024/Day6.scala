package year_2024

import io.Source

import year_2022.Helpers._

@main
def day6(): Unit = {
  val dirs = Seq(
    (-1, 0),
    (0, 1),
    (1, 0),
    (0, -1))

  val grid = Source.fromFile("resources/2024/6").getLines.toArray.map(_.toCharArray)
  var guardPos = (for (i <- grid.indices; j <- grid(i).indices if grid(i)(j) == '^')
    yield (i, j)).head
  grid(guardPos._1)(guardPos._2) = '.'
  val visited = collection.mutable.Set(guardPos)
  var curDirIdx = 0
  def dir = dirs(curDirIdx % 4)
  def curTile = safeGet(grid, guardPos._1, guardPos._2)
  while (curTile.isDefined) {
    while (curTile == Some('.')) {
      visited += guardPos
      guardPos += dir
    }
    if (curTile == Some('#')) {
      guardPos -= dir
      curDirIdx += 1
    }
  }

  println(visited.size)
}
