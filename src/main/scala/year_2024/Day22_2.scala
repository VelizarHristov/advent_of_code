package year_2024

import io.Source

@main
def day22_2(): Unit = {
  def mixPrune(a: Long, b: Long) = (a ^ b) % 16777216
  def toSecret(x: Long) = {
    val x2 = mixPrune(x, x * 64)
    val x3 = mixPrune(x2, x2 / 32)
    mixPrune(x3, x3 * 2048)
  }

  val maps = Source.fromFile("resources/2024/22").getLines.map(_.toLong).map(start => {
    val prices = (1 to 2000)
      .scanLeft(start)((n, _) => toSecret(n))
      .map(_.toInt % 10)
    val diffs = prices.tail.zip(prices).map(_ - _)
    // reverse handles duplicate values for the same sequence of 4 diffs:
    // .toMap keeps only the last occurrence, and we need the first occurrence
    diffs.sliding(4).zip(prices.drop(4)).toVector.reverse.toMap.withDefaultValue(0)
  }).toVector

  val allDiffs = maps.flatMap(_.keySet).distinct
  val res = allDiffs.map(diff =>
    maps.map(_(diff)).sum
  ).max
  println(res)
}
