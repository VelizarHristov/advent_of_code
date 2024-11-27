package year_2022

import io.Source

@main
def day23(): Unit = {
  var grid = (for {
    (line, y) <- Source.fromFile("resources/2022/23").getLines.zipWithIndex
    (char, x) <- line.zipWithIndex
    if char == '#'
  } yield {
    (x, y)
  }).toSet
  /*
  def debugGrid: Unit = {
    val minX = grid.map(_._1).min
    val minY = grid.map(_._2).min
    val nextGrid = grid.map((x, y) => (x - minX, y - minY))
    val maxX = nextGrid.map(_._1).max
    val maxY = nextGrid.map(_._2).max
    for (y <- 0 to maxY)
      for (x <- 0 to maxX)
        if (nextGrid.contains((x, y)))
          print('#')
        else
          print('.')
      println()
    println()
  }
  */
  def toProposal(x: Int, y: Int, roundIdx: Int) = {
    val positions = Seq((0, -1), (0, 1), (-1, 0), (1, 0))
    val validMoves = for {
      (dx, dy) <- (positions ++ positions).drop(roundIdx % 4).take(4)
      checkPos = (-1 to 1).map(i => if (dx == 0) (x + i, y + dy) else (x + dx, y + i)).toSet
      if (checkPos & grid).isEmpty
    } yield (x + dx, y + dy)
    if (validMoves.size == 4 || validMoves.size == 0) {
      // will always be the only proposal there, so will always be accepted
      // that is equivalent to not making a proposal
      (x, y)
    } else {
      validMoves.head
    }
  }
  for (roundIdx <- 0 until 10) {
    val clashingProposals = grid.toVector.map((x, y) => toProposal(x, y, roundIdx))
      .groupBy(p => p).values.filter(_.size > 1).map(_.head).toSet
    grid = grid.map((x, y) =>
      val proposal = toProposal(x, y, roundIdx)
      if (clashingProposals.contains(proposal))
        (x, y)
      else
        proposal
    )
  }
  val minX = grid.map(_._1).min
  val minY = grid.map(_._2).min
  val maxX = grid.map(_._1).max
  val maxY = grid.map(_._2).max
  val res = (maxX - minX + 1) * (maxY - minY + 1) - grid.size

  println(res)
}
