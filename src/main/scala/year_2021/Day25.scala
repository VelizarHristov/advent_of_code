package year_2021

import io.Source

@main
def day25(): Unit = {
  var grid = Source.fromFile("resources/2021/25").getLines.map(_.toCharArray).toArray
  var ticks = 0
  var hasMoved = true
  val nextGrid = grid.map(_.clone())
  while (hasMoved) {
    hasMoved = false
    grid = nextGrid.map(_.clone())
    for (i <- grid.indices; j <- grid(i).indices; if grid(i)(j) == '>') {
      val next = (j + 1) % grid(i).length
      if (grid(i)(next) == '.') {
        nextGrid(i)(next) = '>'
        nextGrid(i)(j) = '.'
        hasMoved = true
      }
    }
    grid = nextGrid.map(_.clone())
    for (i <- grid.indices; j <- grid(i).indices; if grid(i)(j) == 'v') {
      val next = (i + 1) % grid.length
      if (grid(next)(j) == '.') {
        nextGrid(next)(j) = 'v'
        nextGrid(i)(j) = '.'
        hasMoved = true
      }
    }
    ticks += 1
  }
  println(ticks)
}
