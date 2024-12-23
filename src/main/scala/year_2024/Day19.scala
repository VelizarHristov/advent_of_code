package year_2024

import io.Source

@main
def day19(): Unit = {
  val input = Source.fromFile("resources/2024/19").getLines.toVector
  val allPatterns = input.head.split(", ").toSet
  def doesMatch(str: String, patterns: Set[String]): Boolean = {
    patterns.contains(str) ||
      1.until(str.size).exists(len => {
        val (head, tail) = str.splitAt(len)
        patterns.contains(head) && doesMatch(tail, patterns)
      })
  }
  val filteredPatterns = allPatterns.filter(str => {
    !doesMatch(str, allPatterns - str)
  })
  val res = input.drop(2).count(doesMatch(_, filteredPatterns))

  println(res)
}
