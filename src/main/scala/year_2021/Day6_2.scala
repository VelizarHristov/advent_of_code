package year_2021

import scala.io.Source

@main
def day6_2(): Unit = {
  var fishOfDays = Array.fill(9)(0L)
  for (fish <- Source.fromFile("resources/2021/6").getLines.next.split(",").map(_.toInt))
    fishOfDays(fish) += 1
  for (_ <- 1 to 256) {
    val atZero = fishOfDays.head
    fishOfDays = fishOfDays.tail :+ 0
    fishOfDays(6) += atZero
    fishOfDays(8) += atZero
  }
  println(fishOfDays.sum)
}
