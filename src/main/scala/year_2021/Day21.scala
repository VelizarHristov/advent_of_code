package year_2021

import scala.io.Source

@main
def day21(): Unit = {
  val positions = Source.fromFile("resources/21").getLines.map(line => {
    line.reverse.takeWhile(_.isDigit).reverse.toInt
  }).toArray
  val scores = Array.fill(positions.length)(0)
  var nextDice = 1
  var totalRolls = 0
  while (true) {
    for (player <- positions.indices) {
      var totalRoll = 0
      for (_ <- 1 to 3) {
        totalRoll += nextDice
        nextDice = (nextDice % 100) + 1
      }
      val nextPos = ((positions(player) + totalRoll - 1) % 10) + 1
      scores(player) += nextPos
      positions(player) = nextPos
      totalRolls += 3
      if (scores(player) >= 1000) {
        val res = scores.min * totalRolls
        println(res)
        System.exit(0)
      }
    }
  }
}
