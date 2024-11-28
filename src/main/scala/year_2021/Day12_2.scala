package year_2021

import scala.io.Source

@main
def day12_2(): Unit = {
  val oneWayEdges = Source.fromFile("resources/12").getLines.map(line => {
    val Array(from, to) = line.split('-')
    (from, to)
  }).toList
  val edges = (oneWayEdges ++ oneWayEdges.map { case (a, b) => (b, a) })
    .filter { case (a, b) => a != "end" && b != "start" }
  val edgesFrom = edges.groupBy(_._1).view.mapValues(ls => ls.map(_._2))
    .toMap.withDefaultValue(List())

  case class State(current: String, visited: Map[String, Int])
  def next(state: State): Seq[State] = {
    val visitLimit = 3 - state.visited.values.maxOption.getOrElse(1)
    for (next <- edgesFrom(state.current) if state.visited(next) < visitLimit) yield {
      val nextVisited =
        if (next.head.isUpper)
          state.visited
        else
          state.visited + ((next, state.visited(next) + 1))
      State(next, nextVisited)
    }
  }
  var current = Seq(State("start", Map().withDefaultValue(0)))
  var count = 0
  while (current.nonEmpty) {
    current = current.flatMap(next)
    count += current.count(_._1 == "end")
  }
  println(count)
}
