import scala.io.Source
import collection.mutable

import cats.syntax.all.*

@main
def day21_2(): Unit = {
  val initialPositions = Source.fromFile("resources/21").getLines.map(line => {
    line.reverse.takeWhile(_.isDigit).reverse.toInt
  }).toArray
  // for each player:
  // key: (position, score)
  // value: how many worlds are in that state
  val allPositions = List.fill(2)(
    mutable.Map.empty[(Int, Int), BigInt].withDefaultValue(0))
  for (player <- initialPositions.indices)
    allPositions(player)((initialPositions(player), 0)) = 1

  val winningWorlds = Array[BigInt](0, 0)
  var curPlayer = 0
  while (true) {
    val allNext = allPositions(curPlayer).toList
    for (((pos, score), numWorlds) <- allNext if numWorlds != 0) {
      for (diceSum <- Vector.fill(3)((1 to 3).toVector).sequence.map(_.sum)) {
        val nextPos = ((pos + diceSum - 1) % 10) + 1
        val nextScore = score + nextPos
        if (nextScore >= 21)
          winningWorlds(curPlayer) += numWorlds * allPositions((curPlayer + 1) % 2).values.sum
        else
          allPositions(curPlayer)((nextPos, nextScore)) += numWorlds
      }
      allPositions(curPlayer)((pos, score)) -= numWorlds
    }
    if (allPositions(curPlayer).forall(_._2 == 0)) {
      val res = winningWorlds.max
      println(res)
      System.exit(0)
    }
    curPlayer = (curPlayer + 1) % 2
  }
}
