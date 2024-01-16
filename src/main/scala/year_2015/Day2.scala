package year_2015

import io.Source

@main
def day2(): Unit = {
  val res = Source.fromFile("resources/2015/2").getLines.map(line =>
    val sides = line.split('x').map(_.toInt)
    val pairs = sides.zip(sides.tail :+ sides.head).map((a, b) => a * b)
    2 * pairs.sum + pairs.min
  ).sum
  println(res)
}
