package year_2022

import helpers.Helpers.*

import io.Source

import org.saddle.{Vec, Mat}
import org.saddle.ops.BinOps.*
import org.saddle.linalg._

@main
def day22_2(): Unit = {
  val rawInput = Source.fromFile("resources/2022/22").getLines.toArray
  val grid = rawInput.dropRight(2).map(_.toCharArray)
  val sideLen = Math.sqrt(grid.flatten.count(_ != ' ') / 6).toInt
  def getInactiveIdx(pos: Vec[Double]): Int =
    pos.toSeq.indexWhere(p => p == -1 || p == sideLen)

  def move(pos: Vec[Double], dir: Vec[Double]): (Vec[Double], Vec[Double]) = {
    val activeDir = dir.toSeq.indexWhere(_ != 0)
    val newPos = pos + dir
    if (newPos(activeDir) == -1 || newPos(activeDir) == sideLen) {
      val magnitude = if (pos.toSeq.contains(-1)) 1 else -1
      val newDir = Vec(0d, 0d, 0d).updated(getInactiveIdx(pos), magnitude)
      (newPos + newDir, newDir)
    } else {
      (newPos, dir)
    }
  }

  // Note: if inactiveDirIsAtZero then it is at -1 (not 0), else it is at sideLen
  case class Plane(grid: Array[Array[Char]], xInInput: Int, yInInput: Int,
                   transformM: Mat[Double], inactiveDirIsAtZero: Boolean) {
    val starts: Vec[Double] = Vec(transformM.rows.map(rows => {
      if (rows.toSeq.sum > 0) 0d else sideLen.toDouble - 1
    })*)
    val fullT: Mat[Double] = Mat(starts +: transformM.cols*)
    def toXY(pos: Vec[Double]): Vec[Double] = fullT mv Vec(1d).concat(pos)
    val dir1: Vec[Double] = transformM.rows(0)
    val dir2: Vec[Double] = transformM.rows(1)
    val inactiveIdx: Int = (dir1 + dir2).toSeq.indexWhere(_ == 0)
  }
  val planes: Vector[Plane] = {
    val firstGrid = grid.take(sideLen).map(_.dropWhile(_ == ' ').take(sideLen))
    val (firstX, firstY) = (grid.head.takeWhile(_ == ' ').length, 0)
    var gridsSoFar = Vector(Plane(firstGrid, firstX, firstY, Mat(
      Vec(1d, 0d),
      Vec(0d, 1d),
      Vec(0d, 0d)
    ), true))
    var nextGridIdx = 0
    while (nextGridIdx < 6) {
      val plane@Plane(_, x, y, transformM, _) = gridsSoFar(nextGridIdx)
      nextGridIdx += 1
      for {
        (xChange, yChange, movedDirRef) <- Seq(
          (-sideLen, 0, Vec(-1d, 0d)),
          (sideLen, 0, Vec(1d, 0d)),
          (0, sideLen, Vec(0d, 1d)))
        startX = x + xChange
        startY = y + yChange
        if !gridsSoFar.map(s => (s.xInInput, s.yInInput)).contains((startX, startY))
        if safeGet(grid, startY, startX).exists(_ != ' ')
      } {
        val nextGrid = grid.slice(startY, startY + sideLen).map(_.slice(startX, startX + sideLen))
        val movedDir = transformM.T mv movedDirRef
        val movedIdx = movedDir.toSeq.indexWhere(_ != 0)
        val movedDirIsNegative = movedDir(movedIdx) == -1
        val planeEnd = Vec(0d, 0d, 0d).updated(plane.inactiveIdx,
          if (plane.inactiveDirIsAtZero) -1 else sideLen
        ).updated(movedIdx,
          if (movedDirIsNegative) 0 else sideLen - 1)
        val (_, nextDir) = move(planeEnd, movedDir)
        // this multiplies by -1 if xChange is -sideLen
        val nextDirAdjusted = nextDir * movedDirRef.toSeq.find(_ != 0).get
        val newRows = transformM.rows.updated(movedDirRef.toSeq.indexWhere(_ != 0), nextDirAdjusted)
        val newTransformM = Mat(newRows*).T
        gridsSoFar :+= Plane(nextGrid, startX, startY, newTransformM, movedDir.toSeq.contains(-1d))
      }
    }
    gridsSoFar
  }

  // right: Boolean means right on the grid in our data; which is left in a mathematical plane
  def rotate(dir: Vec[Double], plane: Plane, right: Boolean): Vec[Double] = {
    val Seq(dir1Idx, dir2Idx) = plane.transformM.rows.map(_.toSeq.indexWhere(_ != 0))
    val signs = plane.transformM.rows.map(r => if (r.toSeq.contains(-1d)) -1 else 1)
    val rotDirMult = (if (right) -1 else 1) * signs.product
    dir.updated(dir1Idx, dir(dir2Idx) * rotDirMult)
      .updated(dir2Idx, dir(dir1Idx) * -rotDirMult)
  }

  def getPlane(pos: Vec[Double]): Plane = {
    planes.find(p =>
      p.inactiveIdx == getInactiveIdx(pos) &&
        p.inactiveDirIsAtZero == pos.toSeq.contains(-1d)
    ).get
  }

  var pos = Vec(0d, 0d, -1d)
  // the raw input has a number at the start and at the end
  var rawMoves = "R" + rawInput.last
  var dir = Vec(1d, 0d, 0d)
  dir = rotate(dir, planes.head, false)
  while (rawMoves.nonEmpty) {
    val plane = getPlane(pos)
    dir = rotate(dir, plane, rawMoves.head == 'R')
    val lenStr = rawMoves.tail.takeWhile(_.isDigit)
    rawMoves = rawMoves.substring(1 + lenStr.length)
    var remainingMoves = lenStr.toInt
    while (remainingMoves != 0) {
      val (nextPos, nextDir) = move(pos, dir)
      val plane = getPlane(nextPos)
      val Seq(xInGrid, yInGrid) = plane.toXY(nextPos).toSeq.map(_.round.toInt)
      if (plane.grid(yInGrid)(xInGrid) == '.') {
        pos = nextPos
        dir = nextDir
      }
      remainingMoves -= 1
    }
  }
  val plane = getPlane(pos)
  val Seq(xInGrid, yInGrid) = plane.toXY(pos).toSeq.map(_.round.toInt)
  val x = xInGrid + plane.xInInput
  val y = yInGrid + plane.yInInput
  val dirAsXY = (plane.transformM mv dir).toSeq.map(_.round.toInt)
  val dirValue = Seq(
    Seq(1, 0), Seq(0, 1), Seq(-1, 0), Seq(0, -1)
  ).indexOf(dirAsXY)

  val res = (y + 1) * 1000 + (x + 1) * 4 + dirValue
  println(res)
}
