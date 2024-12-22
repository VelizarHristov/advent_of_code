package year_2024

import io.Source

import helpers.Helpers.safeGet

@main
def day4_2(): Unit = {
  val grid = Source.fromFile("resources/2024/4").getLines.toArray.map(_.toCharArray())
  val matchPositions = for {
    i <- grid.indices
    j <- grid(i).indices
    xWords = for {
      mult <- Seq(-1, 1)
      dx <- Seq(-1, 1)
      dy = dx * mult
    } yield (-1 to 1).flatMap(
      k => safeGet(grid, i + dx * k, j + dy * k)
    ).mkString
    if xWords.count(_ == "MAS") == 2
  } yield (i, j)
  println(matchPositions.size)
}
