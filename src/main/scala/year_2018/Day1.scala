package year_2018

import io.Source

@main
def day1(): Unit = {
  val input = Source.fromFile("resources/2018/1").getLines.map(_.toInt)
  println(input.sum)
}
