package year_2024

import io.Source

import year_2022.Helpers.safeGet

@main
def day4(): Unit = {
  val grid = Source.fromFile("resources/2024/4").getLines.toArray.map(_.toCharArray())
  val matchPositions = for {
    i <- grid.indices
    j <- grid(i).indices
    dir <- for {
      dx <- Seq(-1, 0, 1)
      dy <- Seq(-1, 0, 1)
      if (dx, dy) != (0, 0)
      word = (0 until 4).flatMap(k =>
        safeGet(grid, i + dx * k, j + dy * k)
      ).mkString
      if word == "XMAS"
    } yield (dy, dx)
  } yield (i, j, dir)
  println(matchPositions.size)
}
