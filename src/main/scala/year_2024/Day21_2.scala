package year_2024

import io.Source
import collection.mutable

import helpers.Helpers._

@main
def day21_2(): Unit = {
  val MAX_DEPTH = 25
  val numpad = List("789", "456", "123", "!0A").zipWithIndex.flatMap((str, y) => {
    str.zipWithIndex.map((c, x) => ((x, y), c)).filter(_._2 != '!')
  }).toMap
  val controls = List("!^A", "<v>").zipWithIndex.flatMap((str, y) => {
    str.zipWithIndex.map((c, x) => ((x, y), c)).filter(_._2 != '!')
  }).toMap
  val controlToDir = Map('^' -> (0, -1), '<' -> (-1, 0), 'v' -> (0, 1), '>' -> (1, 0))
  val numpadCoords = numpad.map(_.swap)
  val btnCoords = controls.map(_.swap)
  val dirToCoords = controlToDir.map(_.swap)
  // depth - at MAX_DEPTH is controlled by human; at 0 controls the numpad directly
  // lowerCoords - the coordinates of the robot one depth below (all deeper robots are at 'A')
  // returns the number of human presses to reach `to` and press the button there
  val pathLengthMem = Array.fill(MAX_DEPTH + 1)(mutable.Map.empty[((Int, Int), (Int, Int), (Int, Int)), Long])
  def pathLength(depth: Int, from: (Int, Int), to: (Int, Int),
                 lowerCoords: (Int, Int) = btnCoords('A')): Long = {
    val validCoords = if (depth == 0) numpad.keySet else controls.keySet
    pathLengthMem(depth).get((from, to, lowerCoords)) match {
      case Some(l) => l
      case None =>
        val res = {
          if (depth == MAX_DEPTH) {
            1 + (from - to).toList.map(_.abs).sum.toLong
          } else if (from == to) {
            pathLength(depth + 1, lowerCoords, btnCoords('A'))
          } else {
            val (distX, distY) = to - from
            val dx = if (distX != 0) {
              val dir = if (distX > 0) 1 else -1
              Some((dir, 0))
            } else None
            val dy = if (distY != 0) {
              val dir = if (distY > 0) 1 else -1
              Some((0, dir))
            } else None
            Seq(dx, dy).flatten.filter(d => validCoords(d + from)).map(step => {
              val nextLowerCoords = btnCoords(dirToCoords(step))
              val steps = pathLength(depth + 1, lowerCoords, nextLowerCoords)
              steps + pathLength(depth, from + step, to, nextLowerCoords)
            }).min
          }
        }
        pathLengthMem(depth)((from, to, lowerCoords)) = res
        res
    }
  }

  val res = Source.fromFile("resources/2024/21").getLines.map(code => {
    val coords = ("A" + code).map(numpadCoords)
    val totalMoves = coords.zip(coords.tail)
      .map((from, to) => pathLength(0, from, to)).sum
    code.take(3).toInt * totalMoves
  }).sum

  println(res)
}
