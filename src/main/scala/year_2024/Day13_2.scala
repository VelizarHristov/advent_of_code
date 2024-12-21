package year_2024

import io.Source

@main
def day13_2(): Unit = {
  val res = Source.fromFile("resources/2024/13").getLines.grouped(4).map(input => {
    val Seq(Array(dxA, dyA), Array(dxB, dyB)) = input.take(2)
      .map(_.drop("Button n: ".length).split(", ").map(_.drop(2).toInt))
    val Array(prizeX, prizeY) = input(2).drop("Prize: ".length).split(", ").map(n => BigInt(10000000000000l) + n.drop(2).toInt)
    // The way this is derived is with a bunch of algebra:
    // a * dxa + b * dxb = x
    // a * dya + b * dyb = y
    // divide both sides:
    // (a * dxa + b * dxb) / (a * dya + b * dyb) = x/y
    // in a few more transformations we get a/b
    // the numerator of a/b is aPressesRaw, the denominator is bPressesRaw
    val aPressesRaw = dyB * prizeX - dxB * prizeY
    val bPressesRaw = dxA * prizeY - dyA * prizeX
    val gcd = aPressesRaw.gcd(bPressesRaw)
    // we simplify the fraction
    val aPresses = aPressesRaw / gcd
    val bPresses = bPressesRaw / gcd
    val dx = aPresses * dxA + bPresses * dxB
    val dy = aPresses * dyA + bPresses * dyB
    // aPresses and bPresses are the only way to exactly approach the target
    // they will always exist, but are sometimes a non-integer fraction of button presses
    if (prizeX % dx == 0) {
      val pressMult = prizeX / dx
      if (pressMult * dx == prizeX && pressMult * dy == prizeY) {
        pressMult * (aPresses * 3 + bPresses)
      } else {
        BigInt(0)
      }
    } else {
      BigInt(0)
    }
  }).sum

  println(res)
}
