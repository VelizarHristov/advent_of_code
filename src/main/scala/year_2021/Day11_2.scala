package year_2021

import scala.io.Source
import collection.mutable.ArraySeq

@main
def day11_2(): Unit = {
  val octopi = Source.fromFile("resources/11").getLines.to(ArraySeq).map(_.split("").map(_.toInt).to(ArraySeq))
  var steps = 0
  while (octopi.flatten.exists(_ != 0)) {
    var willFlash = Set.empty[(Int, Int)]
    for (i <- octopi.indices; j <- octopi(i).indices) {
      octopi(i)(j) += 1
      if (octopi(i)(j) == 10)
        willFlash += ((i, j))
    }
    while (willFlash.nonEmpty) {
      val next = willFlash
      willFlash = Set.empty
      for {
        (x, y) <- next
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
        if !(dx == 0 && dy == 0)
        if safeGet(octopi, x + dx, y + dy).nonEmpty
      } {
        octopi(x + dx)(y + dy) += 1
        if (octopi(x + dx)(y + dy) == 10) {
          willFlash += ((x + dx, y + dy))
        }
      }
    }
    for (i <- octopi.indices; j <- octopi(i).indices; if octopi(i)(j) >= 10)
      octopi(i)(j) = 0
    steps += 1
  }
  println(steps)
}
