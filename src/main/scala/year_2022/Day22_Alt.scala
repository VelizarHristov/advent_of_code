package year_2022

import helpers.Helpers.*

import io.Source

@main
def day22_Alt(): Unit = {
  val rawInput = Source.fromFile("resources/2022/22").getLines.toArray
  val rawGrid = rawInput.dropRight(2)
  val rowLen = rawGrid.map(_.length).max + 2
  val emptyRow = Array.fill(rowLen)(' ')
  val grid = emptyRow +: rawGrid.map(row =>
    ' ' +: row.toCharArray.padTo(rowLen - 1, ' ')
  ) :+ emptyRow
  val dirs = Array((0, 1), (1, 0), (0, -1), (-1, 0))

  var pos = (1, grid(1).indexOf('.'))
  // the raw input has a number at the start and at the end
  var rawMoves = "R" + rawInput.last + ""
  var curDirIdx = -1
  def dir = dirs(wrapMod(curDirIdx, 4))
  while (rawMoves.nonEmpty) {
    curDirIdx += (if (rawMoves.head == 'R') 1 else -1)
    val rawLen = rawMoves.tail.takeWhile(_.isDigit)
    rawMoves = rawMoves.substring(1 + rawLen.length)
    for (_ <- 1 to rawLen.toInt) {
      var nextPos = pos + dir
      val (y, x) = nextPos
      if (grid(y)(x) == ' ') then
        dir match
          case (0, 1) => nextPos = (y, grid(y).indexWhere(_ != ' '))
          case (1, 0) => nextPos = (grid.indexWhere(row => row(x) != ' '), x)
          case (0, -1) => nextPos = (y, grid(y).lastIndexWhere(_ != ' '))
          case (-1, 0) => nextPos = (grid.lastIndexWhere(row => row(x) != ' '), x)
          case _ => throw new IllegalStateException("Program bug")
      val (y1, x1) = nextPos
      grid(y1)(x1) match {
        case '.' => pos = nextPos
        case _ =>
      }
    }
  }

  val res = pos._1 * 1000 + pos._2 * 4 + curDirIdx % 4
  println(res)
}
