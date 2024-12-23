package year_2024

import io.Source

import helpers.Helpers._

@main
def day8_2(): Unit = {
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
    mult <- (0 until (grid.size max grid(0).size))
    y = y0 + mult * dy
    x = x0 + mult * dx
    if safeGet(grid, y, x).isDefined
  } yield (y, x)
  println(antinodes.distinct.size)
}
