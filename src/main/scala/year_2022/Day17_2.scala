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
  val present = collection.mutable.Set((0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0))
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
    var positions = rockNum % 5 match {
      case 0 => Seq((2, startY), (3, startY), (4, startY), (5, startY))
      case 1 => Seq((3, startY), (2, startY + 1), (4, startY + 1), (3, startY + 2))
      case 2 => Seq((2, startY), (3, startY), (4, startY), (4, startY + 1), (4, startY + 2))
      case 3 => Seq((2, startY), (2, startY + 1), (2, startY + 2), (2, startY + 3))
      case 4 => Seq((2, startY), (3, startY), (2, startY + 1), (3, startY + 1))
    }
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
