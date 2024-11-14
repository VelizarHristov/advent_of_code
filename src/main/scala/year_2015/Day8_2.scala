package year_2015

import collection.mutable
import io.Source

@main
def day8_2(): Unit = {
  val res = Source.fromFile("resources/2015/8").getLines.map(line => {
    var cur = 1
    var sum = 4
    while (cur < line.length - 1) {
      if (line(cur) != '\\') {
        cur += 1
      } else if (line(cur + 1) != 'x') {
        cur += 2
        sum += 2
      } else {
        cur += 4
        sum += 1
      }
    }
    println((line, sum))
    sum
  }).sum
  println(res)
}
