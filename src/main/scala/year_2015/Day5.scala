package year_2015

import io.Source

@main
def day5(): Unit = {
  val vowels = "aeiou"
  val forbidden = Set("ab", "cd", "pq", "xy").map(str => (str(0), str(1)))
  val res = Source.fromFile("resources/2015/5").getLines.count(str => {
    val z = str.zip(str.tail)
    str.count(vowels.contains) >= 3 &&
      z.exists((a, b) => a == b) &&
      !z.exists(forbidden.contains)
  })
  println(res)
}
