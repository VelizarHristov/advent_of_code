package year_2019

import helpers.Helpers.wrapMod
import helpers.Helpers._

@main
def day11_2(): Unit =
  val dirs = Seq((0, -1), (1, 0), (0, 1), (-1, 0))
  val program = IntcodeComputer.fromInputs("11")
  var curPos = (0, 0)
  var curDirIdx = 0
  def dir = dirs(curDirIdx)
  var whiteTiles = Set[(Int, Int)]((0, 0))

  while (program.command != 99)
    val inputs = Seq(if whiteTiles.contains(curPos) then 1L else 0L)
    program.runUntil(inputs, (_, _, out) => out.size == 2) match
      case Vector(newColor, direction) =>
        if newColor == 0 then
          whiteTiles -= curPos
        else
          whiteTiles += curPos
        val dirChange = if direction == 1 then 1 else -1
        curDirIdx = wrapMod(curDirIdx + dirChange.toInt, 4)
        curPos += dir
      case _ =>

  val allX = whiteTiles.map(_._1)
  val allY = whiteTiles.map(_._2)
  for (y <- allY.min to allY.max)
    for (x <- allX.min to allX.max)
      print(if whiteTiles((x, y)) then '#' else '.')
    println()
