package year_2024

import io.Source

import helpers.Helpers.*

@main
def day6_2(): Unit = {
  val dirs = Seq(
    (-1, 0),
    (0, 1),
    (1, 0),
    (0, -1))

  val grid = Source.fromFile("resources/2024/6").getLines.toArray.map(_.toCharArray)
  val initialGuardPos = (for (i <- grid.indices; j <- grid(i).indices if grid(i)(j) == '^')
    yield (i, j)).head
  grid(initialGuardPos._1)(initialGuardPos._2) = '.'
  val allTries = (for {
    newY <- grid.indices
    newX <- grid(newY).indices
    if initialGuardPos != (newY, newX)
    if grid(newY)(newX) == '.'
  } yield {
    grid(newY)(newX) = '#'
    var guardPos = initialGuardPos
    val turnedAt = collection.mutable.Set[((Int, Int), (Int, Int))]()
    var curDirIdx = 0
    def dir = dirs(curDirIdx % 4)
    def curTile = safeGet(grid, guardPos._1, guardPos._2)
    def atTurn = turnedAt((guardPos, dir))
    while (curTile.isDefined && !atTurn) {
      while (curTile == Some('.') && !atTurn) {
        guardPos += dir
      }
      if (curTile == Some('#')) {
        guardPos -= dir
        turnedAt += ((guardPos, dir))
        curDirIdx += 1
      }
    }
    grid(newY)(newX) = '.'
    Option.when(atTurn)((newY, newX))
  }).flatten

  println(allTries.size)
}
