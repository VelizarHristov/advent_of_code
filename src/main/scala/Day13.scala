import scala.io.Source

@main
def day13(): Unit = {
  def display(grid: Array[Array[Boolean]]): Unit = {
    for (y <- grid.indices) {
      for (x <- grid(y).indices) {
        if (grid(y)(x))
          print("#")
        else
          print(".")
      }
      println()
    }
  }

  val input = Source.fromFile("resources/13").getLines
  val dots = input.takeWhile(_.contains(',')).map(line => {
    val Array(x, y) = line.split(",").map(_.toInt)
    (x, y)
  }).toList
  val maxX = dots.map(_._1).max
  val maxY = dots.map(_._2).max
  var grid = Array.fill(maxY + 1, maxX + 1)(false)
  for ((x, y) <- dots) {
    grid(y)(x) = true
  }
  val foldStr = input.next.drop(11)
  val foldAxis = foldStr.head
  val foldCoord = foldStr.drop(2).toInt
  if (foldAxis == 'y') {
    for {
      y <- foldCoord until grid.length
      x <- grid(y).indices
      if grid(y)(x)
    } {
      grid(2 * foldCoord - y)(x) = true
    }
    grid = grid.take(foldCoord)
  } else {
    for {
      y <- grid.indices
      x <- foldCoord until grid(y).length
      if grid(y)(x)
    } {
      grid(y)(2 * foldCoord - x) = true
    }
    grid = grid.map(_.take(foldCoord))
  }
  println(grid.flatten.count(identity))
}
