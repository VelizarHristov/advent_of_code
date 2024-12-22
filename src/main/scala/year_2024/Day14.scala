package year_2024

import io.Source

import year_2022.Helpers.wrapMod

@main
def day14(): Unit = {
  val filename = "resources/2024/14"
  val bounds =
    if (filename.endsWith("test"))
      Array(11, 7)
    else
      Array(101, 103)
  val initialRobots = Source.fromFile(filename).getLines.map(line => {
    val Array(pos, vel) = line.split(' ').map(_.drop(2).split(',').map(_.toInt))
    (pos, vel)
  }).toVector
  val finalPositions = initialRobots.map((pos, vel) =>
    bounds.indices.map(i => wrapMod(pos(i) + vel(i) * 100, bounds(i))))
  val quadrants = for (y <- Array(0, bounds(1) / 2 + 1); x <- Array(0, bounds(0) / 2 + 1)) yield Array(x, y)
  val quadrantCounts = quadrants.map(edge =>
    finalPositions.count(pos =>
        (0 to 1).forall(i =>
          edge(i) <= pos(i) &&
          pos(i) < edge(i) + bounds(i) / 2)))
  val res = quadrantCounts.product

  println(res)
}
