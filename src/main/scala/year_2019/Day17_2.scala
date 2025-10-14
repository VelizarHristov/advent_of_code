package year_2019

import cats.syntax.traverse.toTraverseOps

import collection.mutable

import helpers.Helpers._
import helpers.Helpers.safeGetT

@main
def day17_2(): Unit =
  val code = IntcodeComputer.fromInputs("17").code
  val output = IntcodeComputer(code.clone()).run(List()).map(_.toChar).mkString
  val grid = output.split('\n').map(_.toCharArray())
  code(0) = 2

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
  val input = inputStr.map(_.toLong).toVector
  val res = IntcodeComputer(code).run(input).last
  println(res)
