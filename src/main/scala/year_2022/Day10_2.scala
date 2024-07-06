package year_2022

import io.Source

@main
def day10_2(): Unit = {
  var cycle = 0
  var x = 1
  var toDraw = ""
  def incrementCycle(): Unit = {
    if (x - 1 <= cycle && cycle <= x + 1)
      toDraw += '#'
    else
      toDraw += '.'
    cycle += 1
    if (cycle % 40 == 0) {
      println(toDraw)
      toDraw = ""
      cycle = 0
    }
  }
  for (line <- Source.fromFile("resources/2022/10").getLines) {
    incrementCycle()
    if (line.startsWith("addx")) {
      incrementCycle()
      val num = line.drop(5).toInt
      x += num
    }
  }
}
