package year_2015

import io.Source

@main
def day9_2(): Unit = {
  val adj = Source.fromFile("resources/2015/9").getLines.map(line => {
    val Array(src, rest) = line.split(" to ")
    val Array(dst, value) = rest.split(" = ")
    (Set(src, dst), value.toInt)
  }).toMap
  var maxPath = -1
  val allPaths = adj.keySet.flatten.toVector.permutations
  for (path <- allPaths) {
    val pathLen = path.zip(path.tail).map((s, d) => adj(Set(s, d))).sum
    maxPath = maxPath max pathLen
  }
  println(maxPath)
}
