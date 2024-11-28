import scala.io.Source

@main
def day7(): Unit = {
  val crabs = Source.fromFile("resources/7").getLines.next.split(",").map(_.toInt).toBuffer
  val res = (0 to crabs.max).map(pos => {
    crabs.map(crab => (crab - pos).abs).sum
  }).min
  println(res)
}
