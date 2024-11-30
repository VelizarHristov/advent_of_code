package year_2021

import cats.syntax.all.*

import annotation.{tailrec, targetName}
import collection.mutable
import io.Source

@main // slow runtime - about 60 seconds on my machine
def day19(): Unit = {
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
    beacons.map { p1 =>
      beacons.map { p2 =>
        p1 - p2 }.toSet - XYZ(0, 0, 0) }

  val scanners = mutable.ArrayBuffer[Vector[XYZ]]()
  var skipped = 0
  val input = Source.fromFile("resources/2021/19").getLines.drop(1).toVector
  while (skipped < input.length) {
    scanners += input.drop(skipped).takeWhile(_.nonEmpty).map(line => {
      val Array(x, y, z) = line.split(',').map(_.toInt)
      XYZ(x, y, z)
    })
    skipped += scanners.last.length + 2
  }
  // It seems there are only 24 possibilities instead of all 48 that we are using
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
  def findMatchingBeacons(dists: Vector[Set[XYZ]], nextScannerIdx: Int = 1, rotationIdx: Int = 0): (Vector[XYZ], Int, Int, Int) = {
    if (scanners(nextScannerIdx).isEmpty || rotationIdx == 48) {
      findMatchingBeacons(dists, nextScannerIdx + 1)
    } else {
      val nextDists = rotatedDists(nextScannerIdx)(rotationIdx)
      (for {
        i <- dists.indices.view
        j <- nextDists.indices
        if dists(i).intersect(nextDists(j)).size >= 2
      } yield (i, j)).headOption match {
        case Some((i, j)) => (rotatedScanners(nextScannerIdx)(rotationIdx), nextScannerIdx, i, j)
        case None => findMatchingBeacons(dists, nextScannerIdx, rotationIdx + 1)
      }
    }
  }

  while (scanners.tail.exists(_.nonEmpty)) {
    val dists = beaconsToDists(scanners.head)
    val (rotatedScanner, nextIdx, i, j) = findMatchingBeacons(dists)
    val offset = scanners.head(i) - rotatedScanner(j)
    val adjustedNext = rotatedScanner.map(_ + offset)
    val merged = (scanners.head ++ adjustedNext).distinct
    scanners(0) = merged
    scanners(nextIdx) = Vector()
  }
  val res = scanners.head.size
  println(res)
  val msTaken = System.currentTimeMillis() - timeMsAtStart
  println("Took " + (msTaken / 1000) + "s" + (msTaken % 1000) + "ms")
}
