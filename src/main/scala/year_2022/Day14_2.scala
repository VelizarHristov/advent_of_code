package year_2022

import io.Source

@main
def day14_2(): Unit = {
  val occupied = collection.mutable.Set[(Int, Int)]()
  for (line <- Source.fromFile("resources/2022/14").getLines) {
    val paths = line.split(" -> ").map(str => {
      val Array(x, y) = str.split(",").map(_.toInt)
      (x, y)
    })
    for (((x1, y1), (x2, y2)) <- paths.zip(paths.tail)) {
      for {
        x <- (x1 min x2) to (x1 max x2)
        y <- (y1 min y2) to (y1 max y2)
      } occupied += ((x, y))
    }
  }
  val floorY = occupied.map(_._2).max + 2
  for (x <- 500 - floorY - 1 to 500 + floorY + 1)
    occupied += ((x, floorY))
  val totalRocks = occupied.size

  while (!occupied.contains((500, 0))) {
    var xy = (500, 0)

    while (!occupied.contains(xy)) {
      val (x, y) = xy
      Seq((x, y + 1), (x - 1, y + 1), (x + 1, y + 1)
      ).find(!occupied.contains(_)) match {
        case Some(newXY) => xy = newXY
        case None => occupied += xy
      }
    }
  }

  println(occupied.size - totalRocks)
}
