import scala.io.Source

@main
def day3_2(): Unit = {
  val input = Source.fromFile("resources/3").getLines.toList
  val Seq(o2Rating, co2Rating) = for (preferredBit <- Seq('1', '0')) yield {
    var lines = input
    for (i <- input.head.indices) {
      val lineSize = lines.length - i
      val preferredBitCount = lines.map(_(i)).count(_ == preferredBit)
      val desiredBit =
        if (preferredBitCount == lines.length / 2.0) preferredBit
        else if (preferredBitCount > lines.length / 2.0) '1'
        else '0'
      if (lines.length > 1)
        lines = lines.filter(_(i) == desiredBit)
    }
    Integer.parseInt(lines.head, 2)
  }
  println(o2Rating * co2Rating)
}
