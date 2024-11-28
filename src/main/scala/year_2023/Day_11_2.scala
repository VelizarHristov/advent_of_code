import scala.collection.mutable
import scala.io.Source

@main
def day11_2_2023(): Unit = {
  val grid = Source.fromFile("resources/11_2023").getLines.map(line => {
    line.map(_ == '#').to(mutable.ArraySeq)
  }).to(mutable.ArraySeq)
  val bigRows = grid.transpose.indices.filter(i => grid(i).forall(x => !x)).toSet
  val bigCols = grid.indices.filter(i => grid.transpose.apply(i).forall(x => !x)).toSet
  val galaxyCoords = for {
    i <- grid.indices
    j <- grid(i).indices
    if grid(i)(j)
  } yield (i, j)
  val (bigDistances, smallDistances) = (for {
    a@(y1, x1) <- galaxyCoords
    b@(y2, x2) <- galaxyCoords
    if a != b
  } yield {
    val maxY = y1 max y2
    val minY = y1 min y2
    val maxX = x1 max x2
    val minX = x1 min x2
    val bigRowCount = (minY to maxY).count(bigRows.contains)
    val smallRowCount = maxY - minY - bigRowCount
    val bigColCount = (minX to maxX).count(bigCols.contains)
    val smallColCount = maxX - minX - bigColCount
    (bigRowCount + bigColCount, smallRowCount + smallColCount)
  }).unzip
  val bigSum = bigDistances.sum / 2
  val smallSum = smallDistances.sum / 2
  println(BigInt(bigSum) * 1000000 + smallSum)
}
