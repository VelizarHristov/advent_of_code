import scala.io.Source

@main
def day10_2(): Unit = {
  val brackets = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val valueMap = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  val points = Source.fromFile("resources/10").getLines.map(line => {
    var stack = List.empty[Char]
    var i = 0
    var isCorrupted = false
    while (i < line.length && !isCorrupted) {
      val c = line(i)
      if (brackets.contains(c)) {
        stack = c :: stack
      } else {
        stack.headOption match {
          case Some(b) if brackets(b) == c => stack = stack.tail
          case _ => isCorrupted = true
        }
      }
      i += 1
    }
    if (!isCorrupted && stack.nonEmpty) {
      stack.foldLeft(0L)((acc, next) => 5L * acc + valueMap(brackets(next)))
    } else {
      0
    }
  }).filter(_ != 0).toList
  val res = points.sorted.apply(points.length / 2)
  println(res)
}
