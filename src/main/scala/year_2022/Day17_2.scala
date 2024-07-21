package year_2022

import io.Source

// the cycle happens to be smaller than the total input
// if it needed to go through the entire input multiple times before
// it cycles, then instead I would have needed to use lists for
// lastRockNumDiff and lastStartYDiff
// for example, in the test data each cycle goes through
// 5 times of dirIdx == 0
@main
def day17_2(): Unit = {
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
  val totalRocks = 1000000000000L
  var dirIdx = 0
  var rockNum = 0L
  var skippedLength = 0L

  var lastRockNum = 0L
  var lastStartY = 4L
  var lastRockNumDiff = Long.MaxValue
  var lastStartYDiff = Long.MaxValue

  while (rockNum < totalRocks) {
    val startY = present.map(_._2).max + 4
    var positions = shapes((rockNum % 5).toInt).map { case (x, y) => (x, y + startY) }
    while {
      val nextPos = positions.map { case (x, y) => (x + dirs(dirIdx), y) }
      if (nextPos.forall { case (x, y) => x >= 0 && x <= 6 && !present.contains((x, y)) })
        positions = nextPos
      dirIdx = (dirIdx + 1) % dirs.length
      if (dirIdx == 0) {
        val rockNumDiff = rockNum - lastRockNum
        val startYDiff = startY - lastStartY
        // not guaranteed to work, but should work for this AoC
        if (rockNumDiff == lastRockNumDiff && startYDiff == lastStartYDiff) {
          val skippedCycles = (totalRocks - rockNum) / rockNumDiff
          skippedLength = skippedCycles * startYDiff
          rockNum += skippedCycles * rockNumDiff
        } else {
          lastRockNum = rockNum
          lastStartY = startY
          lastRockNumDiff = rockNumDiff
          lastStartYDiff = startYDiff
        }
      }
      positions.forall { case (x, y) => !present.contains((x, y - 1)) }
    } do {
      positions = positions.map { case (x, y) => (x, y - 1) }
    }
    present ++= positions
    rockNum += 1
  }
  val res = present.map(_._2).max + skippedLength
  println(res)
}
