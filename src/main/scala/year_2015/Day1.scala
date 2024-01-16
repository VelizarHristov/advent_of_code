package year_2015

import io.Source

@main
def day1(): Unit = {
  val input = Source.fromFile("resources/2015/1").getLines.next
  val res = input.length - input.count(_ == ')') * 2
  println(res)
}
