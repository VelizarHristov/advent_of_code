import cats.syntax.all.*

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.io.Source

/**
 * NOTE: This is WIP. Not working right now - abandoned, might return to it at some point.
 */
@main
def day19Faster(): Unit = {
  /*** SKETCH
   *
   * calculate, for all distances pairs for all rotations, ignoring the first scanner:
   * allDists: Map<XYZ, (Int, Int, Int)> - first Int is scanner id, second Int is rotation id, third Int is beacon id
   *
   * when finding matching beacons:
   *   keep a map from (Int, Int) to number of matches
   *   (TODO: do something for the i, j)
   *   for each set of distances belonging to a beacon:
   *     initialize a local map for each (Int, Int, Int) and keep adding to it
   *     then for each that exceeded 2, add to the larger map
   */

  /*** ALT SKETCH
   * Do the opposite - calculate the map for the first beacon then go through each next beacon to look for matches
   */

  /*** ALT SKETCH #2
   * Reuse results of previous steps, both for updating the dists for the main scanner, and for keeping track of matches
   * Actually we can safely throw away any previous dists if we have already done the thing from the first sketch
   */

  val timeMsAtStart = System.currentTimeMillis()
  case class Rotation(signs: Array[Int], indices: Array[Int])
  case class XYZ(x: Int, y: Int, z: Int) {
    lazy val toSeq: Seq[Int] = Seq(x, y, z)
    def transform(r: Rotation): XYZ = {
      val Array(nextX, nextY, nextZ) = r.indices.map(i => toSeq(i) * r.signs(i))
      XYZ(nextX, nextY, nextZ)
    }
    @targetName("add")
    def +(that: XYZ): XYZ = XYZ(x + that.x, y + that.y, z + that.z)
    @targetName("subtract")
    def -(that: XYZ): XYZ = XYZ(x - that.x, y - that.y, z - that.z)
  }
  def beaconsToDists(beacons: Vector[XYZ]): Vector[Set[XYZ]] =
    beacons.map { case XYZ(x1, y1, z1) =>
      beacons.map { case XYZ(x2, y2, z2) =>
        XYZ(x1 - x2, y1 - y2, z1 - z2) }.toSet - XYZ(0, 0, 0) }

  val scanners = mutable.ArrayBuffer[Vector[XYZ]]()
  var skipped = 0
  val input = Source.fromFile("resources/19").getLines.drop(1).toVector
  while (skipped < input.length) {
    scanners += input.drop(skipped).takeWhile(_.nonEmpty).map(line => {
      val Array(x, y, z) = line.split(',').map(_.toInt)
      XYZ(x, y, z)
    })
    skipped += scanners.last.length + 2
  }
  // It seems there are only 24 possibilities instead of all 48
  val allRotations = for {
    signs <- List(List(-1, 1), List(-1, 1), List(-1, 1)).sequence
    indices <- Array(0, 1, 2).permutations
  } yield {
    Rotation(signs.toArray, indices)
  }
  // (i)(j)(k) => ith scanner, jth rotation, kth beacon's XYZ
  val rotatedScanners = Vector(Vector().view) ++ (
    for (scanner <- scanners.tail) yield
      for (rotation <- allRotations.view) yield
        scanner.map(_.transform(rotation)))
  // (i)(j)(k)(l) => ith scanner, jth rotation, kth beacon's set of dists
  val rotatedDists = rotatedScanners.map(scannerRotations => {
    scannerRotations.map(beaconsToDists)
  })

  @tailrec
  def findMatchingBeacons(dists: Vector[Set[XYZ]], nextScannerIdx: Int = 1, rotationIdx: Int = 0): (Vector[XYZ], Int) = {
    if (scanners(nextScannerIdx).isEmpty || rotationIdx == 48) {
      findMatchingBeacons(dists, nextScannerIdx + 1)
    } else {
      val nextDists = rotatedDists(nextScannerIdx)(rotationIdx)
      val matches = for {
        i <- dists.indices.view
        j <- nextDists.indices
        if dists(i).intersect(nextDists(j)).size >= 2
      } yield (i, j)
      // not sure why this is as high as 12, the problem description said it ought to be
      if (matches.take(12).size == 12) {
        // TODO: return all matches (not just first 12) as well as unmatched ones?
        (rotatedScanners(nextScannerIdx)(rotationIdx), nextScannerIdx)
      } else {
        findMatchingBeacons(dists, nextScannerIdx, rotationIdx + 1)
      }
    }
  }

  var curDists = beaconsToDists(scanners.head)
  while (scanners.tail.exists(_.nonEmpty)) {
    val (rotatedScanner, nextIdx) = findMatchingBeacons(curDists)
    // TODO: add new matches to curDists
    scanners(nextIdx) = Vector()
  }
  val res = scanners.head.size
  println(res)
  val msTaken = System.currentTimeMillis() - timeMsAtStart
  println("Took " + (msTaken / 1000) + "s" + (msTaken % 1000) + "ms")
}
