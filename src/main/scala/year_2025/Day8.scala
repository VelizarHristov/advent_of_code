package year_2025

import io.Source

@main
def day8(): Unit =
  val nodes = Source.fromFile("resources/2025/8").getLines.toArray.map:
    _.split(',').map(_.toInt)
  val edges = for
    i <- 0 until nodes.size - 1
    j <- i + 1 until nodes.size
    dist = Math.sqrt(
      nodes(i).zip(nodes(j)).map((a, b) => Math.pow(a - b, 2)).sum
    )
  yield (i, j, dist)
  val usedEdges = edges.sortBy(_._3).take(1000)
  val connections = usedEdges.flatMap((i, j, _) => 
    Seq(i -> List(j), j -> List(i))
  ).groupMapReduce(_._1)(_._2)(_ ++ _).withDefaultValue(List())
  def getConnected(idx: Int, seen: Set[Int] = Set()): Set[Int] =
    connections(idx)
      .filterNot(seen)
      .foldRight(seen + idx)(getConnected(_, _))
  val seen = collection.mutable.Set[Int]()
  var sizes = List[Int]()
  for
    i <- nodes.indices
    if !seen(i)
  do
    val circuit = getConnected(i)
    sizes = circuit.size :: sizes
    seen ++= circuit
  val res = sizes.sortBy(-_).take(3).product
  println(res)
