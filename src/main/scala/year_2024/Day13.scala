package year_2024

import io.Source

@main
def day13(): Unit = {
  val res = Source.fromFile("resources/2024/13").getLines.grouped(4).map(input => {
    val Seq(Array(dxA, dyA), Array(dxB, dyB)) = input.take(2)
      .map(_.drop("Button n: ".length).split(", ").map(_.drop(2).toInt))
    val Array(prizeX, prizeY) = input(2).drop("Prize: ".length).split(", ").map(_.drop(2).toInt)
    (for {
      aPresses <- (0 to 100).view
      bPresses <- (0 to 100).view
      if aPresses * dxA + bPresses * dxB == prizeX
      if aPresses * dyA + bPresses * dyB == prizeY
    } yield 3 * aPresses + bPresses).headOption.getOrElse(0)
  }).sum

  println(res)
}
