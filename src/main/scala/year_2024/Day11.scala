package year_2024

import io.Source

@main
def day11(): Unit = {
  var stones = Source.fromFile("resources/2024/11").getLines.next
    .split(' ').map(_.toLong).toVector
  for (_ <- 1 to 25) {
    stones = stones.flatMap(stone => {
      val digits = stone.toString
      val l = digits.length
      if (stone == 0)
        Vector(1)
      else if (l % 2 == 0)
        digits.splitAt(l / 2).toList.map(_.toLong)
      else
        Vector(stone * 2024)
    })
  }

  println(stones.length)
}
