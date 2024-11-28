package year_2021

import scala.io.Source

@main
def day7_2(): Unit = {
  val crabs = Source.fromFile("resources/7").getLines.next.split(",").map(_.toInt).toBuffer
  val res = (0 to crabs.max).map(pos => {
    crabs.map(crab =>
      (1 to (crab - pos).abs).sum
    ).sum
  }).min
  println(res)
}
