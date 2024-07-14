package year_2022

import io.Source

@main
def day18(): Unit = {
  val cubes = (for (line <- Source.fromFile("resources/2022/18").getLines()) yield {
    val Array(x, y, z) = line.split(",").map(_.toInt)
    (x, y, z)
  }).toSet
  val adjSides = for {
    (x, y, z) <- cubes.view
    (dx, dy, dz) <- Seq((1, 0, 0), (0, 1, 0), (0, 0, 1))
    adjCube = (x + dx, y + dy, z + dz)
    if cubes.contains(adjCube)
  } yield adjCube

  val res = cubes.size * 6 - adjSides.size * 2
  println(res)
}
