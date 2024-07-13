package year_2022

import collection.mutable
import io.Source

@main
def day15(): Unit = {
  val filename = "resources/2022/15"
  val targetY = if (filename.endsWith("test")) 10 else 2000000
  val ranges = mutable.ListBuffer[(Int, Int)]()
  val beacons = mutable.Set[Int]()
  for (line <- Source.fromFile(filename).getLines.toSeq) {
    val numbers = line.zipWithIndex.filter(_._1 == '=').map { case (_, idx) =>
      line.substring(idx + 1).takeWhile(c => c == '-' || c.isDigit).toInt
    }
    val Seq(sensorX, sensorY, beaconX, beaconY) = numbers
    val dist = (sensorX - beaconX).abs + (sensorY - beaconY).abs
    val radiusAroundX = dist - (targetY - sensorY).abs
    if (radiusAroundX >= 0)
      ranges += ((sensorX - radiusAroundX, sensorX + radiusAroundX))
    if (beaconY == targetY)
      beacons += beaconX
  }
  var last = Int.MinValue
  var totalLen = 0
  for ((start, end) <- ranges.sortBy(_._1)) {
    val startToAdd = (last + 1) max start
    val endToAdd = last max end
    totalLen += endToAdd - startToAdd + 1
    for (beacon <- beacons)
      if ((startToAdd to endToAdd).contains(beacon))
        totalLen -= 1
    last = last max end
  }

  println(totalLen)
}
