package year_2015

import io.Source

@main
def day1_2(): Unit = {
  val input = Source.fromFile("resources/2015/1").getLines.next
  var i = 0
  var floor = 0
  while (floor != -1) {
    if (input(i) == '(')
      floor += 1
    else
      floor -= 1
    i += 1
  }
  println(i)
}
