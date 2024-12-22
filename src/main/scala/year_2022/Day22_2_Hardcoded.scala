package year_2022

import helpers.Helpers.*

import io.Source

def getWraparoundMapping(sideLen: Int, isTest: Boolean):
Map[((Int, Int), (Int, Int)), ((Int, Int), (Int, Int))] = {
  val faces = if (isTest) {
    Map(
      1 -> (0, sideLen * 2),
      3 -> (sideLen, sideLen * 2),
      2 -> (sideLen, sideLen),
      4 -> (sideLen, 0),
      6 -> (sideLen * 2, sideLen * 2),
      5 -> (sideLen * 2, sideLen * 3))
  } else {
    Map(
      1 -> (0, sideLen),
      5 -> (0, sideLen * 2),
      3 -> (sideLen, sideLen),
      6 -> (sideLen * 2, sideLen),
      2 -> (sideLen * 2, 0),
      4 -> (sideLen * 3, 0))
  }
  enum Side:
    case Left, Right, Top, Bottom
  val sideMapping = if (isTest) {
    Seq(
      (1, Side.Top)    -> (4, Side.Top, -1),
      (1, Side.Left)   -> (2, Side.Top, 1),
      (1, Side.Right)  -> (5, Side.Right, -1),
      (3, Side.Right)  -> (5, Side.Top, -1),
      (2, Side.Bottom) -> (6, Side.Left, -1),
      (4, Side.Left)   -> (5, Side.Bottom, -1),
      (4, Side.Bottom) -> (6, Side.Bottom, -1))
  } else {
    Seq(
      (1, Side.Top)    -> (4, Side.Left, 1),
      (1, Side.Left)   -> (2, Side.Left, -1),
      (5, Side.Top)    -> (4, Side.Bottom, 1),
      (5, Side.Right)  -> (6, Side.Right, -1),
      (5, Side.Bottom) -> (3, Side.Right, 1),
      (3, Side.Left)   -> (2, Side.Top, 1),
      (6, Side.Bottom) -> (4, Side.Right, 1))
  }
  val oneWayMapping = sideMapping.flatMap { case ((fromFace, fromSide), (toFace, toSide, toMult)) =>
    def sideToCoords(face: Int, side: Side, mult: Int):
    (Int, Int, Int, Int, (Int, Int)) = {
      val (y, x) = faces(face)
      val offset = if (mult == 1) 0 else sideLen - 1
      side match {
        case Side.Left => (y + offset, x, mult, 0, (0, -1))
        case Side.Right => (y + offset, x + sideLen - 1, mult, 0, (0, 1))
        case Side.Top => (y, x + offset, 0, mult, (-1, 0))
        case Side.Bottom => (y + sideLen - 1, x + offset, 0, mult, (1, 0))
      }
    }

    val (fromY, fromX, fromDy, fromDx, fromDir) = sideToCoords(fromFace, fromSide, 1)
    val (toY, toX, toDy, toDx, toDir) = sideToCoords(toFace, toSide, toMult)
    for (i <- 0 until sideLen) yield {
      val startPos = (fromY + fromDy * i, fromX + fromDx * i)
      val endPos = (toY + toDy * i, toX + toDx * i)
      (startPos, fromDir) -> (endPos, toDir)
    }
  }.toMap
  oneWayMapping ++ oneWayMapping.map(_.swap)
}

@main
def day22_2_hardcoded(): Unit = {
  val filename = "22"
  val rawInput = Source.fromFile("resources/2022/" + filename).getLines.toArray
  val grid = rawInput.dropRight(2).map(_.toCharArray)
  val sideLen = Math.sqrt(grid.flatten.count(_ != ' ') / 6).toInt
  val wraparoundMapping = getWraparoundMapping(sideLen, filename == "22_test")
  val dirs = Array((0, 1), (1, 0), (0, -1), (-1, 0))

  var pos = (0, grid.head.indexOf('.'))
  // the raw input has a number at the start and at the end
  var rawMoves = "R" + rawInput.last
  var dir = dirs.last

  while (rawMoves.nonEmpty) {
    val dirInc = if (rawMoves.head == 'R') 1 else -1
    dir = dirs(wrapMod(dirs.indexOf(dir) + dirInc, 4))
    val lenStr = rawMoves.tail.takeWhile(_.isDigit)
    rawMoves = rawMoves.substring(1 + lenStr.length)
    var remainingMoves = lenStr.toInt
    while (remainingMoves != 0) {
      val ((y, x), newDirInv) = wraparoundMapping.getOrElse((pos, dir), (pos + dir, -dir))
      val newDir = -newDirInv
      grid(y)(x) match {
        case '.' =>
          pos = (y, x)
          dir = newDir
          remainingMoves -= 1
        case '#' =>
          remainingMoves = 0
        case _ =>
          throw new IllegalStateException
      }
    }
  }

  val res = (pos._1 + 1) * 1000 + (pos._2 + 1) * 4 + dirs.indexOf(dir)
  println(res)
}
