package year_2021

import scala.io.Source

@main
def day20(): Unit = {
  val input = Source.fromFile("resources/2021/20").getLines.toVector
  val transformation = input.head
  val initialGrid = input.drop(2)
  val iterations = 2
  val offset = iterations * 2
  var grid = Array.fill(initialGrid.size + offset * 2)(Array.fill(initialGrid.size + offset * 2)('.'))
  val nextGrid = Array.fill(initialGrid.size + offset * 2)(Array.fill(initialGrid.size + offset * 2)('.'))
  for (i <- initialGrid.indices; j <- initialGrid(i).indices)
    grid(offset + i)(offset + j) = initialGrid(i)(j)

  for (_ <- 1 to iterations) {
    for {
      i <- 1 until grid.length - 1
      j <- 1 until grid.head.length - 1
    } {
      var sum = 0
      for {
        yOffset <- -1 to 1
        xOffset <- -1 to 1
      } {
        val bitPos = 8 - (yOffset + 1) * 3 - xOffset - 1
        if (grid(i + yOffset)(j + xOffset) == '#')
          sum |= 1 << bitPos
      }
      nextGrid(i)(j) = transformation(sum)
    }
    grid = nextGrid.map(_.clone())
  }

  val res = grid.slice(iterations, offset + iterations + initialGrid.length).flatMap(line =>
    line.slice(iterations, offset + iterations + initialGrid.length)).count(_ == '#')
  println(res)
}
