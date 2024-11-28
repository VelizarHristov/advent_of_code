package year_2021

import scala.io.Source

@main
def day9_2(): Unit = {
  val input = Source.fromFile("resources/2021/9").getLines.toArray.map(_.map(_.toString.toInt))
  val basins = (for (x <- input.indices; y <- input(x).indices) yield {
    val point = input(x)(y)
    val isLow = Seq((-1, 0), (1, 0), (0, 1), (0, -1)).forall {
      case (dx, dy) =>
        val adjPoint = safeGet(input, x + dx, y + dy, 9)
        point < adjPoint
    }
    if (isLow) {
      Some((x, y))
    } else {
      None
    }
  }).flatten
  val basinSizes = for (startPoint <- basins) yield {
    var next = Seq(startPoint)
    var visited = next.toSet
    while(next.nonEmpty) {
      next = (for ((x, y) <- next; (dx, dy) <- Seq((-1, 0), (1, 0), (0, 1), (0, -1))) yield {
        val adjPoint = safeGet(input, x + dx, y + dy, 9)
        if (adjPoint == 9 || visited.contains((x + dx, y + dy)))
          None
        else
          Some((x + dx, y + dy))
      }).flatten
      visited ++= next
    }
    visited.size
  }
  val res = basinSizes.sorted.takeRight(3).product
  println(res)
}
