package year_2015

import io.Source

@main
def day2_2(): Unit = {
  val res = Source.fromFile("resources/2015/2").getLines.map(line =>
    val sides = line.split('x').map(_.toInt)
    val smallestSideFace = 2 * sides.sorted.take(2).sum
    smallestSideFace + sides.product
  ).sum
  println(res)
}
