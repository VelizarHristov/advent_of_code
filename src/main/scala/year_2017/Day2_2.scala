package year_2017

import io.Source

@main
def day2_2(): Unit = {
  val results = Source.fromFile("resources/2017/2").getLines.map(line => {
    val ints = line.split("\\s+").map(_.toInt)
    val (a, b) = (for {
      x <- ints
      y <- ints
      if x > y
      if x % y == 0
    } yield (x, y)).head
    a / b
  })
  println(results.sum)
}
