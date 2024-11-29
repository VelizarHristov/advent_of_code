package year_2021

import cats.syntax.all.*
import collection.parallel.CollectionConverters._

import annotation.{tailrec, targetName}
import collection.mutable
import io.Source

// Takes about 1 second on my machine
@main
def day19Faster(): Unit = {
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

  val input = Source.fromFile("resources/2021/19").getLines.drop(1).toVector
  val scanners = {
    val buf = mutable.ArrayBuffer[Vector[XYZ]]()
    var skipped = 0
      while (skipped < input.length) {
      buf += input.drop(skipped).takeWhile(_.nonEmpty).map(line => {
        val Array(x, y, z) = line.split(',').map(_.toInt)
        XYZ(x, y, z)
      })
      skipped += buf.last.length + 2
    }
    buf.toVector
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

  var remainingScannerIdx = scanners.indices.tail.toVector
  @tailrec
  def findMatchingBeacons(dists: Map[XYZ, Int], nextScannerIdx: Int = 1):
  (Vector[XYZ], Vector[Set[XYZ]], Int, (Int, Int)) = {
    if (!remainingScannerIdx.contains(nextScannerIdx)) {
      findMatchingBeacons(dists, nextScannerIdx + 1)
    } else {
      (0 until 48).par.map(rotationIdx => {
        val nextDists = rotatedDists(nextScannerIdx)(rotationIdx)
        var numMatches = 0
        var i = 0
        var matchedPair: Option[(Int, Int)] = None
        // not sure why this is as high as 12, the problem description said it ought to be
        while (numMatches < 12 && i < nextDists.size) {
          val nextBeaconDists = nextDists(i).iterator
          val matches = mutable.Set[Int]()
          var foundMatch = false
          while (nextBeaconDists.hasNext && !foundMatch) {
            val next = nextBeaconDists.next()
            for (nextMatch <- dists.get(next)) {
              if (matches.contains(nextMatch)) {
                foundMatch = true
                matchedPair = Some(nextMatch, i)
              } else {
                matches += nextMatch
              }
            }
          }
          if (foundMatch)
            numMatches += 1
          i += 1
        }
        (numMatches == 12, rotationIdx, nextDists, matchedPair)
      }).find(_._1) match {
        case Some((_, rotationIdx, nextDists, matchedPair)) => (rotatedScanners(nextScannerIdx)(rotationIdx), nextDists, nextScannerIdx, matchedPair.get)
        case None => findMatchingBeacons(dists, nextScannerIdx + 1)
      }
    }
  }

  var scanner = scanners.head
  val scannerPositions = mutable.ArrayBuffer(XYZ(0, 0, 0))
  var distToBeacon = (for ((dists, i) <- beaconsToDists(scanner).zipWithIndex; dist <- dists)
    yield dist -> i).toMap

  while (remainingScannerIdx.nonEmpty) {
    val (rotatedScanner, rotatedDists, nextIdx, (i, j)) = findMatchingBeacons(distToBeacon)
    val offset = scanner(i) - rotatedScanner(j)
    val adjustedNext = rotatedScanner.map(_ + offset)

    scanner = (scanner ++ adjustedNext).distinct
    distToBeacon ++= (for {
      (dists, i) <- beaconsToDists(adjustedNext).zipWithIndex
      nextIdx = scanner.indexOf(adjustedNext(i))
      dist <- dists
    } yield dist -> nextIdx).toMap
    remainingScannerIdx = remainingScannerIdx.filter(_ != nextIdx)
    scannerPositions += offset
  }
  val res1 = scanner.size
  val res2 = (for (s1 <- scannerPositions; s2 <- scannerPositions) yield {
    s1.toSeq.zip(s2.toSeq).map((p1, p2) => (p1 - p2).abs).sum
  }).max
  val msTaken = System.currentTimeMillis() - timeMsAtStart
  println("Problem 1 answer: " + res1)
  println("Problem 2 answer: " + res2)
  println("Took " + (msTaken / 1000) + "s" + (msTaken % 1000) + "ms")
}
