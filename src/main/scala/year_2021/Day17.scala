package year_2021

import scala.io.Source

@main
def day17(): Unit = {
  val inputLine = Source.fromFile("resources/17").getLines.next
  val input = "(-?\\d+)".r.findAllIn(inputLine).map(_.toInt).toList
  val yRange = input(2) to input(3)
  if (yRange.min >= 0)
    throw new IllegalArgumentException("not implemented")
  var maxMaxY = 0
  for (startYVel <- 0 to yRange.min.abs) {
    var yVel = startYVel
    var y = 0
    var maxY = 0
    while (!yRange.contains(y) && y >= yRange.start) {
      y += yVel
      yVel -= 1
      maxY = maxY max y
    }
    if (yRange.contains(y))
      maxMaxY = maxMaxY max maxY
  }
  println(maxMaxY)
}
