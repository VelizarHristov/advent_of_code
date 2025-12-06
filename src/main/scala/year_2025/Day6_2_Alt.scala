package year_2025

import io.Source

@main
def day6_2_Alt(): Unit =
  val cols = Source.fromFile("resources/2025/6").getLines
    .map(_.toCharArray).toList.transpose
  val colGroups = Vector.unfold(cols): remaining =>
    val (nextGroup, nextCols) = remaining.span(_.exists(_ != ' '))
    Option.when(remaining.nonEmpty)(nextGroup, nextCols.drop(1))

  val res = colGroups.map(colGroup => {
    val op = colGroup.head.last
    val nums = colGroup.map(_.filter(_.isDigit).mkString.toLong)
    if op == '+' then
      nums.sum
    else
      nums.product
  }).sum
  println(res)
