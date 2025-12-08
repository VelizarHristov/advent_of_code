package year_2025

import io.Source

@main
def day8_2(): Unit =
  val nodes = Source.fromFile("resources/2025/8").getLines.toArray.map:
    _.split(',').map(_.toInt)
  val sortedEdges = (for
    i <- 0 until nodes.size - 1
    j <- i + 1 until nodes.size
    dist = Math.sqrt(
      nodes(i).zip(nodes(j)).map((a, b) => Math.pow(a - b, 2)).sum
    )
  yield (i, j, dist)).sortBy(_._3)
  var lowBound = 1000
  var upBound = 100000
  def middle = (upBound + lowBound) / 2
  while (lowBound != upBound)
    val usedEdges = sortedEdges.take(middle)
    val connections = usedEdges.flatMap((i, j, _) => 
      Seq(i -> List(j), j -> List(i))
    ).groupMapReduce(_._1)(_._2)(_ ++ _).withDefaultValue(List())
    def getConnected(idx: Int, seen: Set[Int] = Set()): Set[Int] =
      connections(idx)
        .filterNot(seen)
        .foldRight(seen + idx)(getConnected(_, _))
    if getConnected(0).size == 1000 then
      upBound = middle
    else
      lowBound = middle + 1
  val (a, b, _) = sortedEdges(middle - 1)
  val res = nodes(a).head.toLong * nodes(b).head
  println(res)
