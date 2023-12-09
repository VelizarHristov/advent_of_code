import scala.io.Source

@main
def day12(): Unit = {
  val oneWayEdges = Source.fromFile("resources/12").getLines.map(line => {
    val Array(from, to) = line.split('-')
    (from, to)
  }).toList
  val edges = oneWayEdges ++ oneWayEdges.map { case (a, b) => (b, a) }
  val edgesFrom = edges.groupBy(_._1).view.mapValues(ls => ls.map(_._2))

  case class State(current: String, visited: Set[String])
  def next(state: State): Seq[State] = {
    for (next <- edgesFrom(state.current) if !state.visited.contains(next)) yield {
      val nextVisited =
        if (next.head.isUpper)
          state.visited
        else
          state.visited + next
      State(next, nextVisited)
    }
  }
  var current = Seq(State("start", Set("start")))
  var count = 0
  while (current.nonEmpty) {
    current = current.flatMap(next)
    count += current.count(_._1 == "end")
  }
  println(count)
}
