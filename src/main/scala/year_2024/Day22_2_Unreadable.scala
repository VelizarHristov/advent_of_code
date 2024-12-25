package year_2024

// Remove some variables and newlines to show off unreadable functional style
@main
def day22_2_Unreadable(): Unit = {
  def mixPrune(a: Long, b: Long) = (a ^ b) % 16777216
  val maps = io.Source.fromFile("resources/2024/22").getLines.map(_.toLong).map(start => {
    val prices = (1 to 2000).scanLeft(start)((x, _) => {
      val x2 = mixPrune(x, x * 64)
      val x3 = mixPrune(x2, x2 / 32)
      mixPrune(x3, x3 * 2048)
    }).map(_.toInt % 10)
    prices.tail.zip(prices).map(_ - _).sliding(4).zip(prices.drop(4)).toVector
      .reverse.toMap.withDefaultValue(0)
  }).toVector
  println(maps.flatMap(_.keySet).distinct.map(nums => maps.map(_(nums)).sum).max)
}
