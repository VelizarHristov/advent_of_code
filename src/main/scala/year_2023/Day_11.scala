package year_2023

import scala.collection.mutable
import scala.io.Source

@main
def day11(): Unit = {
  var grid = Source.fromFile("resources/2023/11").getLines.map(line => {
    line.map(_ == '#').to(mutable.ArraySeq)
  }).to(mutable.ArraySeq)
  var added = 0
  for (i <- grid.indices) {
    if (grid(i + added).forall(i => !i)) {
      grid = (grid.take(i + added) :+ mutable.ArraySeq.fill(grid.head.size)(false)) ++ grid.drop(i + added)
      added += 1
    }
  }

  val colsToExpand = grid.transpose.zipWithIndex.filter { case (a, _) => a.forall(x => !x)}.map(_._2)
  for (i <- grid.indices) {
    var added = 0
    for (colToAdd <- colsToExpand) {
      grid(i) = (grid(i).take(added + colToAdd) :+ false) ++ grid(i).drop(added + colToAdd)
      added += 1
    }
  }
  val galaxyCoords = for {
    i <- grid.indices
    j <- grid(i).indices
    if grid(i)(j)
  } yield (i, j)
  val distances = for {
    (y1, x1) <- galaxyCoords
    (y2, x2) <- galaxyCoords
  } yield (y1 - y2).abs + (x1 - x2).abs
  println(distances.sum / 2)
}
