package year_2021

import scala.io.Source
@main
def day5(): Unit = {
  val coords = Source.fromFile("resources/5").getLines.map(line => {
    "\\d+".r.findAllIn(line).toSeq.map(_.toInt)
  }).toList
  val maxCoordinate = coords.flatten.max
  val lines = coords.map(c => (c(0), c(1), c(2), c(3)))
    .filter { case (x1, y1, x2, y2) => x1 == x2 || y1 == y2 }
  val counts = Array.fill(maxCoordinate + 1, maxCoordinate + 1)(0)
  for (case (x1, y1, x2, y2) <- lines) {
    val dy = y2.compare(y1)
    val dx = x2.compare(x1)
    val length = ((x1 - x2).abs max (y1 - y2).abs) + 1
    for (i <- 0 until length) {
      counts(y1 + dy * i)(x1 + dx * i) += 1
    }
  }
  val res = counts.flatten.count(_ >= 2)
  println(res)
}
