package year_2025

import io.Source

@main
def day7_2(): Unit =
  val grid = Source.fromFile("resources/2025/7").getLines.toArray
  val (startY, startX) = (for
    y <- grid.indices
    x <- grid(y).indices
    if grid(y)(x) == 'S'
  yield (y, x)).head

  var beams = Vector((startX, 1L))
  ((startY + 1) until grid.size).foreach: y =>
    beams = beams.flatMap((x, count) =>
      if grid(y)(x) == '^' then
        Seq((x - 1, count), (x + 1, count))
      else
        Seq((x, count))
    ).groupMapReduce(_._1)(_._2)(_ + _).toVector
  val res = beams.map(_._2).sum
  println(res)
