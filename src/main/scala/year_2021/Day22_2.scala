package year_2021

import io.Source
import cats.syntax.all.*

@main
def day22_2(): Unit = {
  def intersect(r1: Range, r2: Range): Range =
    (r1.start max r2.start) to (r1.last min r2.last)

  def rangeSegments(r1: Range, r2: Range): Vector[Range] = Vector(
    r1.start until r2.start,
    r2,
    r2.last + 1 to r1.last
  ).filter(_.nonEmpty)

  var areas = Vector.empty[Vector[Range]]
  Source.fromFile("resources/2021/22").getLines.foreach(line => {
    val isOn = line.startsWith("on")
    val coords = "-?\\d+".r.findAllIn(line).toVector.map(_.toInt)
    val nextArea = Vector(
      coords(0) to coords(1),
      coords(2) to coords(3),
      coords(4) to coords(5))
    areas = areas.flatMap(presentArea => {
      val intersection = presentArea.zip(nextArea).map(intersect)
      if (intersection.forall(_.nonEmpty)) {
        // remove `intersection` from presentArea, resulting in (up to) 26 segments
        presentArea.zip(intersection)
          .map(rangeSegments)
          .sequence
          .filter(_ != intersection)
      } else {
        Vector(presentArea)
      }
    })
    if (isOn)
      areas :+= nextArea
  })

  val res = areas.map(_.map(r => BigInt(r.size)).product).sum
  println(res)
}
