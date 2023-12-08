import scala.io.Source

@main
def day1_2(): Unit = {
  val input = Source.fromFile("resources/1").getLines.map(_.toInt).toList
  val slidingSum = input.sliding(3).map(_.sum).toList
  val res = slidingSum.zip(slidingSum.tail).count(_ < _)
  println(res)
}
