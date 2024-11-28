package year_2021

import scala.io.Source

@main
def day17_2(): Unit = {
  val inputLine = Source.fromFile("resources/17").getLines.next
  val input = "(-?\\d+)".r.findAllIn(inputLine).map(_.toInt).toList
  val xRange = input(0) to input(1)
  val yRange = input(2) to input(3)
  if (xRange.min <= 0 || yRange.min >= 0)
    throw new IllegalArgumentException("not implemented")

  var count = 0
  for (startXVel <- 0 to xRange.max; startYVel <- yRange.min to yRange.min.abs) {
    var xVel = startXVel
    var yVel = startYVel
    var x = 0
    var y = 0
    while (!(xRange.contains(x) && yRange.contains(y)) && x <= xRange.end && y >= yRange.start) {
      x += xVel
      y += yVel
      if (xVel > 0)
        xVel -= 1
      else if (xVel < 0)
        xVel -= 1
      yVel -= 1
    }
    if (xRange.contains(x) && yRange.contains(y))
      count += 1
  }
  println(count)
}
