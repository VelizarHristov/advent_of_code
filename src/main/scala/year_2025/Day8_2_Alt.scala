package year_2025

import io.Source

import cats.collections.DisjointSets

@main
def day8_2_Alt(): Unit =
  val nodes = Source.fromFile("resources/2025/8").getLines.toArray.map:
    _.split(',').map(_.toInt)
  val sortedEdges = (for
    i <- 0 until nodes.size - 1
    j <- i + 1 until nodes.size
    dist = Math.sqrt(
      nodes(i).zip(nodes(j)).map((a, b) => Math.pow(a - b, 2)).sum
    )
  yield (i, j, dist)).sortBy(_._3).iterator

  var sets = DisjointSets(nodes.indices *)
  var size = 1000
  while (true)
    val (a, b, _) = sortedEdges.next
    if sets.find(a)._2 != sets.find(b)._2 then
      sets = sets.union(a, b)._1
      size -= 1
    if size == 1 then
      val res = nodes(a).head.toLong * nodes(b).head
      println(res)
      System.exit(0)
