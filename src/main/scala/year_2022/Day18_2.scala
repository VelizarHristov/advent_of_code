package year_2022

import io.Source

@main
def day18_2(): Unit = {
  val cubes = (for (line <- Source.fromFile("resources/2022/18").getLines()) yield {
    val Array(x, y, z) = line.split(",").map(_.toInt)
    (x, y, z)
  }).toSet
  val bound = cubes.map { case (x, y, z) => x max y max z }.max
  val searched = collection.mutable.Map[(Int, Int, Int), Boolean]()

  def isOpen(point: (Int, Int, Int)): Boolean = {
    searched.get(point) match {
      case Some(b) => b
      case None =>
        val visited = collection.mutable.Set(point)
        var states = Vector(point)
        var res: Option[Boolean] = None
        while (states.nonEmpty && res == None) {
          val nextStates = collection.mutable.ListBuffer[(Int, Int, Int)]()
          for {
            (x, y, z) <- states
            (dx, dy, dz) <- Seq((1, 0, 0), (0, 1, 0), (0, 0, 1))
            len <- Seq(-1, 1)
            next = (x + dx * len, y + dy * len, z + dz * len)
            if !cubes.contains(next)
            if !visited.contains(next)
            if !nextStates.contains(next)
          } {
            if ((x min y min z) < 0 || (x max y max z) > bound) {
              res = Some(true)
              searched(next) = true
            } else {
              searched.get(next) match {
                case None =>
                  visited += next
                  nextStates += next
                case b =>
                  res = b
              }
            }
          }
          states = nextStates.toVector
        }
        if (states.isEmpty && res == None)
          res = Some(false)
        for (p <- visited)
          searched(p) = res.get
        res.get
    }
  }

  val adjSides = for {
    (x, y, z) <- cubes.view
    (dx, dy, dz) <- Seq((1, 0, 0), (0, 1, 0), (0, 0, 1))
    adjCube = (x + dx, y + dy, z + dz)
    if cubes.contains(adjCube) || !isOpen(adjCube)
  } yield adjCube

  val res = cubes.size * 6 - adjSides.size * 2
  println(res)
}
