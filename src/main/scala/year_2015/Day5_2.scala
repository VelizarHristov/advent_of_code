package year_2015

import io.Source

@main
def day5_2(): Unit = {
  val res = Source.fromFile("resources/2015/5").getLines.count(str => {
    str.sliding(2).exists(pair => {
      str.indexOf(pair) < str.lastIndexOf(pair) - 1
    }) && str.sliding(3).exists(tri => tri(0) == tri(2))
  })
  println(res)
}
