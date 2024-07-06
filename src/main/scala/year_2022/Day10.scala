package year_2022

import io.Source

@main
def day10(): Unit = {
  var cycle = 1
  var x = 1
  var sum = 0
  def incrementCycle(): Unit = {
    if ((cycle + 20) % 40 == 0) {
      sum += cycle * x
    }
    cycle += 1
  }
  for (line <- Source.fromFile("resources/2022/10").getLines) {
    incrementCycle()
    if (line.startsWith("addx")) {
      incrementCycle()
      val num = line.drop(5).toInt
      x += num
    }
  }
  println(sum)
}
