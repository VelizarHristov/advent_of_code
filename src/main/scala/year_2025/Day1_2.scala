package year_2025

import helpers.Helpers.wrapMod

import io.Source

@main
def day1_2(): Unit =
  val input = Source.fromFile("resources/2025/1").getLines
    .map(line => (line.head, line.tail.toInt))
  var cur = 50
  var res = 0
  input.foreach: (dir, count) =>
    (1 to count).foreach: _ =>
      cur += (if (dir == 'R') 1 else -1)
      cur = wrapMod(cur, 100)
      if cur == 0 then
        res += 1
  println(res)
