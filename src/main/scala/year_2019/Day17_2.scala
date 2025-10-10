package year_2019

import cats.syntax.traverse.toTraverseOps

import io.Source
import collection.mutable

import helpers.Helpers._
import helpers.Helpers.safeGetT

@main
def day17_2(): Unit =
  case class Program(prog: Array[Int]):
    var pos = 0
    var relOffset = 0
    def op = prog(pos).toString
    def command = op.takeRight(2).toInt
    def mode(i: Int) =
      val modes = ("0" + op.dropRight(2)).toInt
      (modes / math.pow(10, i)).round % 10
    def param(i: Int): Int =
      val immediateParam = prog(pos + i + 1)
      mode(i) match
        case 0 => prog(immediateParam)
        case 1 => immediateParam
        case 2 => prog(immediateParam + relOffset)
    def numInstructions = command match
      case 1 | 2 | 7 | 8 => 4
      case 3 | 4 | 9 => 2
      case 5 | 6 => 3
    def storeNum(v: Int) =
      val storeAddr = prog(pos + numInstructions - 1)
      if (mode(numInstructions - 2) == 2)
        prog(storeAddr + relOffset) = v
      else
        prog(storeAddr) = v

    def run(inputs: Seq[Int]): Vector[Int] =
      var outputs = Vector[Int]()
      var nextInputIdx = 0
      def input =
        val res = inputs(nextInputIdx)
        nextInputIdx += 1
        res

      while (command != 99)
        var toStore: Option[Int] = None
        var newPos = pos + numInstructions
        command match
          case 1 => toStore = Some(param(0) + param(1))
          case 2 => toStore = Some(param(0) * param(1))
          case 3 => toStore = Some(input)
          case 4 => outputs = outputs :+ param(0)
          case 5 => if param(0) != 0 then newPos = param(1)
          case 6 => if param(0) == 0 then newPos = param(1)
          case 7 => toStore = if param(0) < param(1) then Some(1) else Some(0)
          case 8 => toStore = if param(0) == param(1) then Some(1) else Some(0)
          case 9 => relOffset += param(0)
        toStore.foreach(storeNum)
        pos = newPos
      outputs

  val prog = 
    val arr = Source.fromFile("resources/2019/17").getLines.next.split(',').map(_.toInt)
    arr.padTo(arr.length * 4, 0)
  val output = Program(prog.clone()).run(List()).map(_.toChar).mkString
  val grid = output.split('\n').map(_.toCharArray())
  prog(0) = 2

  // Find a path
  var pos = (
    for
      y <- grid.indices
      x <- grid(y).indices
      if grid(y)(x) == '^' // sufficient for my data
    yield (y, x)
  ).head
  var dir = (-1, 0)
  val dirs = List((-1, 0), (0, 1), (1, 0), (0, -1))
  def turnDirs = dirs.filter(d => d != dir && d != -dir)
  def posAfterTurn = for
    dir <- turnDirs
    res <- safeGetT(grid, dir + pos)
    if res == '#'
  yield dir + pos
  val pathBuilder = mutable.ArrayBuffer[(Char, Int)]()
  var halt = false
  while (!halt)
    posAfterTurn match
      case _ :: _ :: _ => throw new IllegalStateException("Unsupported data - 2 possible turns")
      case nextPos :: _ =>
        val newDir = nextPos - pos
        val rightTurnDir = dirs((dirs.indexOf(dir) + 1) % 4)
        val turnChar = if newDir == rightTurnDir then 'R' else 'L'
        var moves = 1
        def newPos = pos + newDir * moves
        while (safeGetT(grid, newPos + newDir).contains('#'))
          moves += 1
        pathBuilder.append((turnChar, moves))
        pos = newPos
        dir = newDir
      case Nil => halt = true
  def pathStr(path: Seq[(Char, Int)]) = path.map((c, i) => s"$c,$i").mkString(",")

  type Mapping = Map[Char, Seq[(Char, Int)]]
  type Path = Vector[Either[(Char, Int), Char]]
  def replaceAll(path: Path, pattern: Seq[(Char, Int)], sym: Char): Path =
    path.indexOfSlice(pattern.map(Left.apply)) match
      case -1 => path
      case i => replaceAll(
        path.patch(i, Seq(Right(sym)), pattern.size),
        pattern,
        sym
      )

  def findMappings(
    path: Path,
    mappings: Mapping = Map(),
    remainingLetters: Int = 3
  ): Seq[(Vector[Char], Mapping)] =
    if remainingLetters == 0 then
      path.traverse(_.toSeq)
        .filter(_.length <= 10)
        .map((_, mappings))
    else
      val offset = path.segmentLength(_.isRight)
      val maxLen = path.drop(offset).segmentLength(_.isLeft)
      for
        len <- 1 to maxLen
        usedPath = path.drop(offset).take(len).traverse(_.swap.toOption).get
        if pathStr(usedPath).length <= 20
        sym = "ABC"(3 - remainingLetters)
        nextMap = mappings + ((sym, usedPath))
        // We never try to replace only some - works for my data
        nextPath = replaceAll(path, usedPath, sym)
        solution <- findMappings(nextPath, nextMap, remainingLetters - 1)
      yield solution

  // Find a way to map the path to A,B,C
  val (path, mappings) = findMappings(
    pathBuilder.toVector.map(Left.apply)
  ).head // assume it exists
  val inputStr = path.mkString(",") + '\n' +
    mappings.toList.sortBy(_._1).map(p => pathStr(p._2)).mkString("\n") +
    "\nn\n"
  val input = inputStr.map(_.toInt).toVector
  val res = Program(prog).run(input).last
  println(res)
