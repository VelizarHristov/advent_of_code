package year_2015

import io.Source

@main
def day9(): Unit = {
  val adj = Source.fromFile("resources/2015/9").getLines.map(line => {
    val Array(src, rest) = line.split(" to ")
    val Array(dst, value) = rest.split(" = ")
    (Set(src, dst), value.toInt)
  }).toMap
  val pathCosts = for (path <- adj.keySet.flatten.toVector.permutations) yield
    path.zip(path.tail).map((s, d) => adj(Set(s, d))).sum
  println(pathCosts.min)
}
