package year_2022

import io.Source

@main
def day17(): Unit = {
  val dirs = Source.fromFile("resources/2022/17").getLines().next
    .map(c => if (c == '>') 1 else -1).toArray
  val present = (0 to 7).map((_, 0)).to(collection.mutable.Set)
  val shapes = Array(
"""
####
""",

"""
.#.
#.#
.#.
""", // the middle was removed because it makes no difference

"""
..#
..#
###
""",

"""
#
#
#
#
""",

"""
##
##
""").map(str => {
    val lines = str.split("\n").map(_.toCharArray)
    for (y <- lines.indices; x <- lines(y).indices; if lines(y)(x) == '#')
      yield (x + 2, lines.length - y - 1)
  })
  var dirIdx = 0

  for (rockNum <- 0 until 2022) {
    val startY = present.map(_._2).max + 4
    var positions = shapes(rockNum % 5).map { case (x, y) => (x, y + startY) }
    while {
      val nextPos = positions.map { case (x, y) => (x + dirs(dirIdx), y) }
      if (nextPos.forall { case (x, y) => x >= 0 && x <= 6 && !present.contains((x, y)) })
        positions = nextPos
      dirIdx = (dirIdx + 1) % dirs.length
      positions.forall { case (x, y) => !present.contains((x, y - 1)) }
    } do {
      positions = positions.map { case (x, y) => (x, y - 1) }
    }
    present ++= positions
  }

  println(present.map(_._2).max)
}
