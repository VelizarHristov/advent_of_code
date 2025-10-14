package year_2019

import helpers.Helpers.wrapMod
import helpers.Helpers._

@main
def day11(): Unit =
  val program = IntcodeComputer.fromInputs("11")
  var curPos = (0, 0)
  val dirs = Seq((0, -1), (1, 0), (0, 1), (-1, 0))
  var curDirIdx = 0
  def dir = dirs(curDirIdx)
  var painted = Set[(Int, Int)]()
  var whiteTiles = Set[(Int, Int)]()

  while (program.command != 99)
    val inputs = Seq(if whiteTiles.contains(curPos) then 1L else 0L)
    program.runUntil(inputs, (_, _, out) => out.size == 2) match
      case Vector(newColor, direction) =>
        if newColor == 0 then
          whiteTiles -= curPos
        else
          whiteTiles += curPos
        painted += curPos
        val dirChange = if direction == 1 then 1 else -1
        curDirIdx = wrapMod(curDirIdx + dirChange.toInt, 4)
        curPos += dir
      case _ =>
  println(painted.size)
