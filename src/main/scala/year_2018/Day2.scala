package year_2018

import io.Source

@main
def day2(): Unit = {
  val (hasTwo, hasThree) = Source.fromFile("resources/2018/2").getLines.map(line => {
    val counts = line.distinct.map(c => line.count(_ == c))
    (counts.contains(2), counts.contains(3))
  }).toSeq.unzip
  val twosCount = hasTwo.count(_ == true)
  val threesCount = hasThree.count(_ == true)
  println(twosCount * threesCount)
}
