package year_2024

import io.Source
import collection.mutable

@main
def day23(): Unit = {
  val map = mutable.Map.empty[String, Set[String]]
    .withDefaultValue(Set.empty)
  Source.fromFile("resources/2024/23").getLines.foreach(line => {
    val Array(a, b) = line.split('-')
    map(a) += b
    map(b) += a
  })
  val res = map.map((key, values) => {
    (for {
      v1 <- values
      v2 <- values
      if v1 != v2
      if map(v1)(v2)
    } yield Seq(key, v1, v2).sorted
    ).filter(_.exists(_.startsWith("t")))
  }).toSet.flatten.size

  println(res)
}
