import io.Source

@main
def day10(): Unit = {
  val brackets = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val valueMap = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  var totalValue = 0
  Source.fromFile("resources/10").getLines.foreach(line => {
    var stack = List.empty[Char]
    var i = 0
    var valueOfFound = 0
    while (i < line.length && valueOfFound == 0) {
      val c = line(i)
      if (brackets.contains(c)) {
        stack = c :: stack
      } else {
        stack.headOption match {
          case Some(b) if brackets(b) == c => stack = stack.tail
          case _ => valueOfFound = valueMap(c)
        }
      }
      i += 1
    }
    totalValue += valueOfFound
  })
  println(totalValue)
}
