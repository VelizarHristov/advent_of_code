import scala.io.Source

@main
def day1(): Unit = {
  val input = Source.fromFile("resources/1").getLines.map(_.toInt).toList
  val res = input.zip(input.tail).count(_ < _)
  println(res)
}
