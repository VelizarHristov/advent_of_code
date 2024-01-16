package year_2015

import io.Source
import collection.mutable

@main
def day3(): Unit = {
  val dirToXY = Map('^' -> (0, 1), 'v' -> (0, -1), '>' -> (1, 0), '<' -> (-1, 0))
  val visited = mutable.Set((0, 0))
  var cur = (0, 0)
  Source.fromFile("resources/2015/3").getLines.next.foreach(dir => {
    val (x, y) = cur
    val (dx, dy) = dirToXY(dir)
    cur = (x + dx, y + dy)
    visited += cur
  })
  println(visited.size)
}
