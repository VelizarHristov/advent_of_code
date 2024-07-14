package year_2022

import io.Source

@main
def day17(): Unit = {
  val dirs = Source.fromFile("resources/2022/17").getLines().next
    .map(c => if (c == '>') 1 else -1).toArray
  val present = collection.mutable.Set((0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0))
  var dirIdx = 0

  for (rockNum <- 0 until 2022) {
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
      positions.forall { case (x, y) => !present.contains((x, y - 1)) }
    } do {
      positions = positions.map { case (x, y) => (x, y - 1) }
    }
    present ++= positions
  }

  println(present.map(_._2).max)
}
