package year_2024

import io.Source

import cats.syntax.traverse.toTraverseOps

@main
def day15_2(): Unit = {
  val input = Source.fromFile("resources/2024/15").getLines.toArray
  val grid = input.takeWhile(_.nonEmpty).map(_.toArray.flatMap {
    case '#' => "##"
    case 'O' => "[]"
    case '.' => ".."
    case '@' => "@."
  })
  val moves = input.drop(grid.length + 1).mkString

  // if push is impossible outputs None
  // if push is possible outputs all pushed tiles
  def moveY(tiles: Vector[(Int, Int)], dy: Int): Option[Vector[(Int, Int)]] = {
    val pushedTiles = tiles.map((x, y) => (x, y + dy))
    pushedTiles.traverse((x, y) => grid(y)(x) match {
      case '#' => None
      case '.' => Some(Vector.empty)
      case '[' => Some(Vector((x, y), (x + 1, y)))
      case ']' => Some(Vector((x - 1, y), (x, y)))
    }).flatMap(boxPushResults => {
      boxPushResults.flatten.distinct match {
        case Vector() => Some(tiles)
        case pushedBoxes => moveY(pushedBoxes, dy).map(_ ++ tiles)
      }
    })
  }

  // if push is impossible outputs None
  // if push is possible outputs the x coordinate of the next free tile
  def moveX(x: Int, y: Int, dx: Int): Option[Int] = grid(y)(x) match {
    case '#' => None
    case '.' => Some(x)
    case '[' | ']' => moveX(x + dx, y, dx)
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
    if (dy == 0) {
      for (lastX <- moveX(robotX + dx, robotY, dx)) {
        for (x <- lastX to robotX by -dx)
          grid(robotY)(x) = grid(robotY)(x - dx)
        grid(robotY)(robotX) = '.'
      }
    } else {
      for {
        movedBoxes <- moveY(Vector((robotX, robotY)), dy)
        (boxX, boxY) <- movedBoxes.sortBy(_._2 * -dy)
      } {
        grid(boxY + dy)(boxX) = grid(boxY)(boxX)
        grid(boxY)(boxX) = '.'
      }
    }
  }

  val res = (for (y <- grid.indices; x <- grid(y).indices if grid(y)(x) == '[')
    yield 100 * y + x).sum
  println(res)
}
