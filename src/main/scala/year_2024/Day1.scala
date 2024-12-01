package year_2024

import io.Source

@main
def day1(): Unit = {
  val input = Source.fromFile("resources/2024/1").getLines
    .map(_.split("\\s+").map(_.toInt)).toVector
  val ls1 = input.map(_(0)).sorted
  val ls2 = input.map(_(1)).sorted
  val res = ls1.zip(ls2).map((a, b) =>
    (a - b).abs
  ).sum
  println(res)
}
