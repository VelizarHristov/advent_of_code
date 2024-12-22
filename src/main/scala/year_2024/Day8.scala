package year_2024

import io.Source

import helpers.Helpers._

@main
def day8(): Unit = {
  val grid = Source.fromFile("resources/2024/8").getLines.toArray.map(_.toCharArray)
  val symbols = grid.flatten.distinct.filterNot("#.".contains)
  val symCoords = symbols.map(sym =>
    for (i <- grid.indices; j <- grid(i).indices; if grid(i)(j) == sym)
      yield (i, j))
  val antinodes = for {
    coords <- symCoords
    coord1 <- coords
    coord2 <- coords
    if coord1 != coord2
    (dy, dx) = (coord1 - coord2)
    (y0, x0) = coord1
    y = y0 + dy
    x = x0 + dx
    if safeGet(grid, y, x).isDefined
  } yield (y, x)
  println(antinodes.distinct.size)
}
