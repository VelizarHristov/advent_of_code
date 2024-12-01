package year_2017

import io.Source

@main
def day2(): Unit = {
  val results = Source.fromFile("resources/2017/2").getLines.map(line => {
    val ints = line.split("\\s+").map(_.toInt)
    ints.max - ints.min
  })
  println(results.sum)
}
