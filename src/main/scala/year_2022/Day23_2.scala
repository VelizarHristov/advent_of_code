package year_2022

import io.Source

@main
def day23_2(): Unit = {
  var grid = (for {
    (line, y) <- Source.fromFile("resources/2022/23").getLines.zipWithIndex
    (char, x) <- line.zipWithIndex
    if char == '#'
  } yield {
    (x, y)
  }).toSet
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
  var roundIdx = 0
  while ({
    val prevGrid = grid
    val clashingProposals = grid.toVector.map((x, y) => toProposal(x, y, roundIdx))
      .groupBy(p => p).values.filter(_.size > 1).map(_.head).toSet
    grid = grid.map((x, y) =>
      val proposal = toProposal(x, y, roundIdx)
      if (clashingProposals.contains(proposal))
        (x, y)
      else
        proposal
    )
    roundIdx += 1
    prevGrid != grid
  }) ()
  println(roundIdx)
}
