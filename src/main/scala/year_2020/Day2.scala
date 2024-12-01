package year_2020

import io.Source

@main
def day2(): Unit = {
  val res = Source.fromFile("resources/2020/2").getLines.count(line => {
    val Array(countsStr, letterStr, password) = line.split(' ')
    val Array(minCount, maxCount) = countsStr.split('-').map(_.toInt)
    val letter = letterStr.head
    val actualCount = password.count(_ == letter)
    minCount <= actualCount && actualCount <= maxCount
  })
  println(res)
}
