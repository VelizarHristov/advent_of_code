package year_2025

import io.Source

@main
def day9(): Unit =
  val nodes = Source.fromFile("resources/2025/9").getLines.toArray.map: line =>
    val Array(x, y) = line.split(',').map(_.toInt)
    (x, y)
  val areas = for
    (x1, y1) <- nodes
    (x2, y2) <- nodes
    xDiff = (x1 - x2).abs + 1
    yDiff = (y1 - y2).abs + 1
  yield xDiff.toLong * yDiff
  val res = areas.max
  println(res)
