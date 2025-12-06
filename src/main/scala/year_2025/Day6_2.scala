package year_2025

import io.Source

@main
def day6_2(): Unit =
  val input = Source.fromFile("resources/2025/6").getLines
    .map(_.toCharArray).toArray.transpose :+ Array(' ')
  var nums = Vector[Long]()
  var sum = 0L
  var op = '+'
  input.foreach: col =>
    val digits = col.mkString.filter(_.isDigit)
    if digits.isEmpty then
      if op == '+' then
        sum += nums.sum
      else
        sum += nums.product
      nums = Vector()
    else
      nums = nums :+ digits.toLong
    if "+*".contains(col.last) then
      op = col.last
  println(sum)
