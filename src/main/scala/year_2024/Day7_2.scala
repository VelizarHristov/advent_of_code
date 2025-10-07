package year_2024

import io.Source

@main
def day7_2(): Unit = {
  def canEqualTotal(nums: List[Long], total: Long, acc: Long = 0): Boolean = nums match
    case Nil              => total == acc
    case _ if acc > total => false
    case n :: rest        =>
      canEqualTotal(rest, total, acc + n) ||
      canEqualTotal(rest, total, acc * n) ||
      canEqualTotal(rest, total, (acc.toString + n.toString).toLong)

  val res = Source.fromFile("resources/2024/7").getLines.flatMap(line => {
    val Array(firstStr, rest) = line.split(": ")
    val total = firstStr.toLong
    val nums = rest.split(' ').map(_.toLong).toList
    Option.when(canEqualTotal(nums, total))(total)
  }).sum
  println(res)
}
