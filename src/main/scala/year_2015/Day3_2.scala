package year_2015

import collection.mutable
import io.Source

@main
def day3_2(): Unit = {
  val dirToXY = Map('^' -> (0, 1), 'v' -> (0, -1), '>' -> (1, 0), '<' -> (-1, 0))
  val visited = mutable.Set((0, 0))
  val positions = Array((0, 0), (0, 0))
  for (dirs <- Source.fromFile("resources/2015/3").getLines.next.grouped(2); i <- 0 to 1) {
    val (x, y) = positions(i)
    val (dx, dy) = dirToXY(dirs(i))
    positions(i) = (x + dx, y + dy)
    visited += positions(i)
  }
  println(visited.size)
}
