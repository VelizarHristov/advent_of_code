package year_2024

import io.Source

@main
def day11_2(): Unit = {
  var stones = Source.fromFile("resources/2024/11").getLines.next
    .split(' ').map(_.toLong)
    .groupBy(identity).toVector
    .map((num, ls) => (num, ls.length.toLong))
  for (_ <- 1 to 75) {
    stones = stones.flatMap((stone, count) => {
      val digits = stone.toString
      val l = digits.length
      if (stone == 0)
        Vector(1l -> count)
      else if (l % 2 == 0)
        digits.splitAt(l / 2).toList.map(_.toLong -> count)
      else
        Vector(stone * 2024 -> count)
    }).groupBy(_._1)
    .map((num, ls) => (num, ls.map(_._2).sum))
    .toVector
  }

  val res = stones.map(_._2).sum
  println(res)
}
