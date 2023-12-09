import scala.io.Source

@main
def day14(): Unit = {
  val input = Source.fromFile("resources/14").getLines.toArray
  var state = input.head
  val map = input.drop(2).map(line => {
    val Array(from, to) = line.split(" -> ")
    (from, to)
  }).toMap.withDefaultValue("")
  for (_ <- 1 to 10) {
    state = state.sliding(2).flatMap(pair => {
      pair.take(1) + map(pair)
    }).mkString + state.last
  }
  val lengthsMap = state.groupBy(identity).view.mapValues(_.length)
  val res = lengthsMap.maxBy(_._2)._2 - lengthsMap.minBy(_._2)._2
  println(res)
}
