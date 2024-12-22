package year_2024

import io.Source

import helpers.Helpers.safeGet

@main
def day10(): Unit = {
  val dirs = Seq(
    (0, 1),
    (0, -1),
    (-1, 0),
    (1, 0))
  val grid = Source.fromFile("resources/2024/10").getLines.toArray.map(_.map(_.asDigit).toArray)
  def posToResults(y: Int, x: Int): Set[(Int, Int)] = {
    val n = grid(y)(x)
    if (n == 9) {
      Set((y, x))
    } else {
      dirs.flatMap((dy, dx) => {
        val next = safeGet(grid, y + dy, x + dx)
        if (next == Some(n + 1))
          posToResults(y + dy, x + dx)
        else
          Set.empty
      }).toSet
    }
  }
  val res = (for (i <- grid.indices; j <- grid.head.indices if grid(i)(j) == 0) yield
    posToResults(i, j).size).sum

  println(res)
}
