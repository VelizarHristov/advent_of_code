package year_2024

import io.Source

import year_2022.Helpers.safeGet

@main
def day10_2(): Unit = {
  val dirs = Seq(
    (0, 1),
    (0, -1),
    (-1, 0),
    (1, 0))
  val grid = Source.fromFile("resources/2024/10").getLines.toArray.map(_.map(_.asDigit).toArray)
  def posToResults(y: Int, x: Int): Int = {
    val n = grid(y)(x)
    if (n == 9) {
      1
    } else {
      dirs.map((dy, dx) => {
        val next = safeGet(grid, y + dy, x + dx)
        if (next == Some(n + 1))
          posToResults(y + dy, x + dx)
        else
          0
      }).sum
    }
  }
  val res = (for (i <- grid.indices; j <- grid.head.indices if grid(i)(j) == 0) yield
    posToResults(i, j)).sum

  println(res)
}
