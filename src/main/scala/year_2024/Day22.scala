package year_2024

import io.Source

@main
def day22(): Unit = {
  def mixPrune(a: Long, b: Long) = (a ^ b) % 16777216
  def toSecret(x: Long) = {
    val x2 = mixPrune(x, x * 64)
    val x3 = mixPrune(x2, x2 / 32)
    mixPrune(x3, x3 * 2048)
  }
  def toSecretNTimes(x: Long, n: Int): Long = {
    if (n == 0)
      x
    else
      toSecretNTimes(toSecret(x), n - 1)
  }

  val res = Source.fromFile("resources/2024/22").getLines.map(line => {
    toSecretNTimes(line.toInt, 2000)
  }).sum

  println(res)
}
