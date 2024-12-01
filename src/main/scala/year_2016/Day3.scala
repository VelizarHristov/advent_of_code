package year_2016

import io.Source

@main
def day3(): Unit = {
  val res = Source.fromFile("resources/2016/3").getLines.count(line => {
    val nums = line.split("\\s+").tail.map(_.toInt)
    nums.sum > nums.max * 2
  })
  println(res)
}
