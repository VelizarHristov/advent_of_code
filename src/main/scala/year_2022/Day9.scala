package year_2022

import helpers.Helpers._
import io.Source

@main
def day9(): Unit = {
  var head, tail = (0, 0)
  val visited = collection.mutable.Set.empty[(Int, Int)]
  visited += tail
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
      head += headDirection
      for (direction <- dirMap.values)
        if (head == tail + direction * 2)
          tail += direction
      for (xDir <- Seq(-1, 1))
        for (yDir <- Seq(-1, 1))
          for (xLen <- Seq(1, 2)) {
            val yLen = (xLen % 2) + 1
            val movement = (xDir * xLen, yDir * yLen)
            if (head == tail + movement)
              tail += (xDir, yDir)
          }
      visited += tail
    }
  }
  println(visited.size)
}
