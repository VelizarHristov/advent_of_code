package year_2025

import io.Source

@main
def day9_2(): Unit =
  val nodes = Source.fromFile("resources/2025/9").getLines.toArray.map: line =>
    val Array(x, y) = line.split(',').map(_.toInt)
    (x, y)
  val greenLines = nodes.zip(nodes.tail :+ nodes.head).map:
    case ((x1, y1), (x2, y2)) =>
      (x1 min x2, x1 max x2, y1 min y2, y1 max y2)
  val areas = for
    (x1, y1) <- nodes
    (x2, y2) <- nodes
    xDiff = (x1 - x2).abs + 1
    yDiff = (y1 - y2).abs + 1
    dist = xDiff.toLong * yDiff
  yield (x1 min x2, x1 max x2, y1 min y2, y1 max y2, dist)
// Note: only works with our data
// For more generality: start a line in x and in y that reaches our rectangle
//   which should intersect an odd number of green lines
  val bestPair = areas.sortBy(-_._5).find((minX, maxX, minY, maxY, _) =>
    !greenLines.exists: (minX2, maxX2, minY2, maxY2) =>
      minX < maxX2 && minX2 < maxX && minY < maxY2 && minY2 < maxY
  ).get._5
  println(bestPair)
