package year_2021

import scala.io.Source

@main
def day13_2(): Unit = {
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

  val input = Source.fromFile("resources/2021/13").getLines
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
  for (line <- input) {
    val foldAxis = line(11)
    val foldCoord = line.drop(13).toInt
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
  }
  display(grid)
}
