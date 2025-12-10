package year_2025

import io.Source

@main
def day9_2_Alt(): Unit =
  val nodes = Source.fromFile("resources/2025/9").getLines.toArray.map: line =>
    val Array(x, y) = line.split(',').map(_.toInt)
    (x, y)
  val areas = for
    (x1, y1) <- nodes
    (x2, y2) <- nodes
    if !nodes.zip(nodes.tail :+ nodes.head).exists:
      case ((x21, y21), (x22, y22)) =>
        (x1 min x2) < (x21 max x22) && (x21 min x22) < (x1 max x2) && (y1 min y2) < (y21 max y22) && (y21 min y22) < (y1 max y2)
  yield ((x1 - x2).abs + 1).toLong * ((y1 - y2).abs + 1)
  println(areas.max)
