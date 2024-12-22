package year_2024

import io.Source

import helpers.Helpers.wrapMod

@main
def day14_2(): Unit = {
  val bounds = Array(101, 103)
  var robots = Source.fromFile("resources/2024/14").getLines.map(line => {
    val Array(pos, vel) = line.split(' ').map(_.drop(2).split(',').map(_.toInt))
    (pos, vel)
  }).toVector
  var res = 1
  while (true) {
    robots = robots.map((pos, vel) => {
      val nextPos = bounds.indices.map(i => wrapMod(pos(i) + vel(i), bounds(i)))
      (nextPos.toArray, vel)
    })

    // no idea why this worked
    // it wasn't in the problem description and it doesn't even make sense
    val positions = robots.map(_._1.toVector).distinct
    if (positions.size == robots.size) {
      val grid = Array.fill(bounds(1))(Array.fill(bounds(0))('.'))
      for (Vector(x, y) <- positions)
        grid(y)(x) = '#'
      for (line <- grid)
        println(line.mkString)
      println(res)
      System.exit(0)
    }
    res += 1
  }
}
