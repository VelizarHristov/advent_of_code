package year_2022

import helpers.Helpers._
import io.Source

@main
def day9_2(): Unit = {
  val knots = Array.fill(10)((0, 0))
  val visited = collection.mutable.Set.empty[(Int, Int)]
  visited += knots.last
  val dirMap = Map(
    'L' -> (-1, 0),
    'R' -> (1, 0),
    'U' -> (0, -1),
    'D' -> (0, 1),
  )

  for (line <- Source.fromFile("resources/2022/9").getLines) {
    val headDirection = dirMap(line(0))
    val len = line.drop(2).toInt
    for (_ <- 1 to len) {
      knots(0) += headDirection
      for (tailId <- 1 until knots.length) {
        for (direction <- dirMap.values)
          if (knots(tailId - 1) == knots(tailId) + direction * 2)
            knots(tailId) += direction
        for (xDir <- Seq(-1, 1))
          for (yDir <- Seq(-1, 1))
            for (xLen <- Seq(1, 2)) {
              val yLen = (xLen % 2) + 1
              val movement = (xDir * xLen, yDir * yLen)
              if ((knots(tailId - 1) == knots(tailId) + movement)
               || (knots(tailId - 1) == (knots(tailId) + (xDir, yDir) * 2))) {
                knots(tailId) += (xDir, yDir)
              }
            }
      }
      visited += knots.last
    }
  }
  println(visited.size)
}
