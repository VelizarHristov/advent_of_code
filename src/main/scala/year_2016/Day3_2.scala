package year_2016

import io.Source

@main
def day3_2(): Unit = {
  val res = Source.fromFile("resources/2016/3").getLines.grouped(3).map(lines => {
    lines.map(_.split("\\s+").tail.map(_.toInt)).transpose.count(nums => {
      nums.sum > nums.max * 2
    })
  }).sum
  println(res)
}
