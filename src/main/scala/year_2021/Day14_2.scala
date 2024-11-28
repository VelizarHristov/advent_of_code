package year_2021

import scala.io.Source

@main
def day14_2(): Unit = {
  val input = Source.fromFile("resources/2021/14").getLines.toArray
  var state = input.head.sliding(2).toList.groupBy(identity).view.mapValues(_.length.toLong).toMap
  val map = input.drop(2).map(line => {
    val Array(from, to) = line.split(" -> ")
    (from, Seq(from.take(1) + to, to + from.drop(1)))
  }).toMap.withDefault(str => Seq(str))
  for (_ <- 1 to 40) {
    val next = for {
      (key, value) <- state.toList
      nextKey <- map(key)
    } yield nextKey -> value
    state = next.groupBy(_._1).view.mapValues(v => v.map(_._2).sum).toMap
  }
  val counts = state.toList
    .flatMap((k, v) => Seq(k.head -> v, k.last -> v))
    .groupBy(_._1).values
    .map(v => (v.map(_._2).sum / 2.0).ceil.toLong)
  val res = counts.max - counts.min
  println(res)
}
