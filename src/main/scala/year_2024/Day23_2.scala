package year_2024

import io.Source
import collection.mutable

@main
def day23_2(): Unit = {
  val map = mutable.Map.empty[String, Set[String]]
    .withDefaultValue(Set.empty)
  val checked: mutable.Set[Set[String]] = mutable.Set.empty
  def largestGroup(startSet: Set[String]): Set[String] = {
    checked += startSet
    startSet.view.flatMap(map)
      .filter(x => (startSet & map(x)) == startSet)
      .map(startSet + _)
      .filterNot(checked)
      .map(largestGroup)
      .maxByOption(_.size)
      .getOrElse(startSet)
  }
  Source.fromFile("resources/2024/23").getLines.foreach(line => {
    val Array(a, b) = line.split('-')
    map(a) += b
    map(b) += a
  })
  val largest = map.map(x => largestGroup(Set(x._1))).maxBy(_.size)

  val res = largest.toVector.sorted.mkString(",")
  println(res)
}
