package year_2024

import io.Source

@main
def day15(): Unit = {
  val input = Source.fromFile("resources/2024/15").getLines.toArray
  val grid = input.takeWhile(_.nonEmpty).map(_.toArray)
  val moves = input.drop(grid.length + 1).mkString
  def moveTo(x: Int, y: Int, dx: Int, dy: Int): Option[(Int, Int)] = grid(y)(x) match {
    case '#' => None
    case '.' => Some((x, y))
    case 'O' => moveTo(x + dx, y + dy, dx, dy)
  }

  for (move <- moves) {
    val robotPos@(robotX, robotY) = (for (
      y <- grid.indices.view; x <- grid(y).indices if grid(y)(x) == '@'
    ) yield (x, y)).head
    val (dx, dy) = move match {
      case '<' => (-1, 0)
      case '>' => (1, 0)
      case '^' => (0, -1)
      case 'v' => (0, 1)
    }
    val nextRobotX = robotX + dx
    val nextRobotY = robotY + dy
    for ((dstX, dstY) <- moveTo(nextRobotX, nextRobotY, dx, dy)) {
      grid(dstY)(dstX) = 'O'
      grid(robotY)(robotX) = '.'
      grid(nextRobotY)(nextRobotX) = '@'
    }
  }

  val res = (for (y <- grid.indices; x <- grid(y).indices if grid(y)(x) == 'O')
    yield 100 * y + x).sum
  println(res)
}
