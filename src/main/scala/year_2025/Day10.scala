package year_2025

import io.Source

@main
def day10(): Unit =
  val res = Source.fromFile("resources/2025/10").getLines.toArray.map(line => {
    val sections = line.split(' ')
    val req = sections.head.tail.init.zipWithIndex.filter((c, _) => c == '#').map(_._2).toSet
    val btns = sections.tail.init.map(_.tail.init.split(',').map(_.toInt).toSet).toSet
    btns.subsets.filter(
      _.fold(req)((s1, s2) => (s1 -- s2) ++ (s2 -- s1)).isEmpty
    ).map(_.size).min
  }).sum
  println(res)
