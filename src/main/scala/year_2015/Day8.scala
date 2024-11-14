package year_2015

import io.Source

@main
def day8(): Unit = {
  val res = Source.fromFile("resources/2015/8").getLines.map(line => {
    var cur = 1
    var sum = 2
    while (cur < line.length - 1) {
      if (line(cur) != '\\') {
        cur += 1
      } else if (line(cur + 1) != 'x') {
        cur += 2
        sum += 1
      } else {
        cur += 4
        sum += 3
      }
    }
    sum
  }).sum
  println(res)
}
