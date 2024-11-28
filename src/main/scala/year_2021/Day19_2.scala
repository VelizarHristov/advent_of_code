package year_2021

import cats.syntax.all.*

import annotation.{tailrec, targetName}
import collection.mutable
import io.Source

@main // slow runtime - about 200 seconds on my machine
def day19_2(): Unit = {
  val timeMsAtStart = System.currentTimeMillis()
  case class Rotation(signs: Seq[Int], indices: Seq[Int])
  case class XYZ(x: Int, y: Int, z: Int) {
    lazy val toSeq: Seq[Int] = Seq(x, y, z)
    def transform(r: Rotation): XYZ = {
      val Seq(nextX, nextY, nextZ) = r.indices.map(i => toSeq(i) * r.signs(i))
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
  val input = Source.fromFile("resources/2021/19").getLines.drop(1).toVector
  while (skipped < input.length) {
    scanners += input.drop(skipped).takeWhile(_.nonEmpty).map(line => {
      val Array(x, y, z) = line.split(',').map(_.toInt)
      XYZ(x, y, z)
    })
    skipped += scanners.last.length + 2
  }
  // There might be only 24 possibilities instead of all 48
  val allRotations = for {
    signs <- Vector(Vector(-1, 1), Vector(-1, 1), Vector(-1, 1)).sequence
    indices <- List(0, 1, 2).permutations
  } yield {
    Rotation(signs, indices)
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
      val matches = for {
        i <- dists.indices.view
        j <- nextDists.indices
        if dists(i).intersect(nextDists(j)).size >= 2
      } yield (i, j)
      // not sure why this is as high as 12, the problem description said it ought to be
      if (matches.take(12).size == 12) {
        val (i, j) = matches.head
        (rotatedScanners(nextScannerIdx)(rotationIdx), nextScannerIdx, i, j)
      } else {
        findMatchingBeacons(dists, nextScannerIdx, rotationIdx + 1)
      }
    }
  }

  val scannerPositions = mutable.ArrayBuffer(XYZ(0, 0, 0))
  while (scannerPositions.size < scanners.size) {
    val dists = beaconsToDists(scanners.head)
    val (rotatedScanner, nextIdx, i, j) = findMatchingBeacons(dists)
    val offset = scanners.head(i) - rotatedScanner(j)
    val adjustedNext = rotatedScanner.map(_ + offset)
    val merged = (scanners.head ++ adjustedNext).distinct
    scanners(0) = merged
    scanners(nextIdx) = Vector()
    scannerPositions += offset
  }
  val dists = for (s1 <- scannerPositions; s2 <- scannerPositions) yield {
    s1.toSeq.zip(s2.toSeq).map((p1, p2) => (p1 - p2).abs).sum
  }
  println(dists.max)
  val msTaken = System.currentTimeMillis() - timeMsAtStart
  println("Took " + (msTaken / 1000) + "s" + (msTaken % 1000) + "ms")
}
