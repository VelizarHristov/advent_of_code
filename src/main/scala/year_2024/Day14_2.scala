package year_2024

import io.Source

import year_2022.Helpers.wrapMod

@main
def day14_2(): Unit = {
  val bounds = Array(101, 103)
  var robots = Source.fromFile("resources/2024/14").getLines.map(line => {
    val Array(pos, vel) = line.split(' ').map(_.drop(2).split(',').map(_.toInt))
    (pos, vel)
  }).toVector
  var i = 1
  while (true) {
    robots = robots.map((pos, vel) => {
      val nextPos = bounds.indices.map(i => wrapMod(pos(i) + vel(i), bounds(i)))
      (nextPos.toArray, vel)
    })

    val grid = Array.fill(bounds(1))(Array.fill(bounds(0))('.'))
    for ((Array(x, y), _) <- robots)
      grid(y)(x) = '#'
    // no idea why this worked
    // it wasn't in the problem description and it doesn't even make sense
    if (grid.flatten.count(_ == '#') == robots.size) {
      for (line <- grid) {
        println(line.mkString)
      }
      println(i)
      System.exit(0)
    }
    i += 1
  }
}
