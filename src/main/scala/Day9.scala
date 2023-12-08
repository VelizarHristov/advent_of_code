import scala.io.Source

@main
def day9(): Unit = {
  val input = Source.fromFile("resources/9").getLines.toArray.map(_.map(_.toString.toInt))
  val res = (for (x <- input.indices; y <- input(x).indices) yield {
    val point = input(x)(y)
    val isLow = Seq((-1, 0), (1, 0), (0, 1), (0, -1)).forall {
      case (dx, dy) =>
        val adjPoint = input.lift(x + dx).getOrElse(Seq())
          .lift(y + dy).getOrElse(9)
        point < adjPoint
    }
    if (isLow)
      point + 1
    else
      0
  }).sum
  println(res)
}
