package year_2022

import io.Source

@main
def day15_2(): Unit = {
  val filename = "resources/2022/15"
  val bounds = if (filename.endsWith("test")) 20 else 4000000
  val sensors = (for (line <- Source.fromFile(filename).getLines.toSeq) yield {
    val numbers = line.zipWithIndex.filter(_._1 == '=').map { case (_, idx) =>
      line.substring(idx + 1).takeWhile(c => c == '-' || c.isDigit).toInt
    }
    val Seq(sensorX, sensorY, beaconX, beaconY) = numbers
    val dist = (sensorX - beaconX).abs + (sensorY - beaconY).abs
    ((sensorX, sensorY), dist)
  }).toList

  def findMinYScore(x: Int): (Int, Int) = {
    var minScore = Int.MaxValue
    var yAtMinScore = 0
    var score = 0
    var y = 0
    var remainingSensors = sensors.map { case ((sensorX, sensorY), dist) =>
      (dist - (sensorX - x).abs + 1, sensorY)
    }.sortBy { case (score, sensorY) => -(score - sensorY) }
    while (remainingSensors.nonEmpty && y < bounds) {
      val (nextScoreBonus, nextY) = remainingSensors.head
      remainingSensors = remainingSensors.tail
      val nextScore = nextScoreBonus - (nextY - y).abs
      if (nextY <= y) {
        score = score max nextScore
      } else {
        if (nextScore < score) {
          if (score - (nextY - y) >= nextScoreBonus) {
            score -= (nextY - y)
            y = nextY min bounds
          } else {
            y += (score - nextScore) / 2
            score = (score + nextScore) / 2
          }
          if (score < minScore) {
            minScore = score
            yAtMinScore = y
          }
        } else {
          score = nextScore
        }
        if (y < nextY)
          y += (nextY - y) * 2
      }
      y = y min bounds
    }
    (yAtMinScore, minScore)
  }

  val timeMsAtStart = System.currentTimeMillis()
  var x = 0
  while (true) {
    val (yAtMinRad, minRad) = findMinYScore(x)
    if (minRad == 0) {
      val res = x * 4000000 + yAtMinRad
      println("Took " + (System.currentTimeMillis() - timeMsAtStart) + "ms")
      println(res)
      System.exit(0)
    }
    x += minRad
  }
}
