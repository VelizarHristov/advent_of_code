package year_2022

import io.Source
import year_2022.Helpers._

@main
def day22(): Unit = {
  val rawInput = Source.fromFile("resources/2022/22").getLines.toArray
  val rawGrid = rawInput.dropRight(2)
  val rowLen = rawGrid.map(_.length).max
  val grid = rawGrid.map(row => row.toCharArray.padTo(rowLen, ' '))
  val dirs = Array((0, 1), (1, 0), (0, -1), (-1, 0))

  var pos = (0, grid.head.indexOf('.'))
  // the raw input has a number at the start and at the end
  var rawMoves = "R" + rawInput.last
  var curDirIdx = -1
  def dir = dirs(wrapMod(curDirIdx, 4))
  while (rawMoves.nonEmpty) {
    println((pos, dir))
    curDirIdx += (if (rawMoves.head == 'R') 1 else -1)
    val lenStr = rawMoves.tail.takeWhile(_.isDigit)
    rawMoves = rawMoves.substring(1 + lenStr.length)
    var remainingMoves = lenStr.toInt
    var lastValidPos = pos
    while (remainingMoves != 0) {
      val (y1, x1) = pos + dir
      val y = wrapMod(y1, grid.length)
      val x = wrapMod(x1, rowLen)
      grid(y)(x) match {
        case '.' =>
          pos = (y, x)
          lastValidPos = pos
          remainingMoves -= 1
        case '#' =>
          remainingMoves = 0
        case _ =>
          pos = (y, x)
      }
    }
    pos = lastValidPos
  }
  println((pos, dir))

  val res = (pos._1 + 1) * 1000 + (pos._2 + 1) * 4 + curDirIdx % 4
  println(res)
}
