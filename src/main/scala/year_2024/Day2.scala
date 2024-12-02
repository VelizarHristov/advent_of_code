package year_2024

import io.Source

@main
def day2(): Unit = {
  val res = Source.fromFile("resources/2024/2").getLines.count(line => {
    val nums = line.split(' ').map(_.toInt)
    val diffs = nums.zip(nums.tail).map((x, y) => x - y)
    diffs.forall(d => d > 0 && d <= 3) ||
      diffs.forall(d => d >= -3 && d < 0)
  })
  println(res)
}
