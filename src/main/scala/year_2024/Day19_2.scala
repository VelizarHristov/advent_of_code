package year_2024

import io.Source

@main
def day19_2(): Unit = {
  val input = Source.fromFile("resources/2024/19").getLines.toVector
  val patterns = input.head.split(", ").toSet
  val cache = collection.mutable.Map.empty[String, Long]
  def matchCount(str: String): Long = {
    cache.get(str) match {
      case Some(v) => v
      case None =>
        val wholeMatchNum = if (patterns.contains(str)) 1 else 0
        val res = 1.until(str.size).map(len => {
          val (head, tail) = str.splitAt(len)
          if (patterns.contains(head))
            matchCount(tail)
          else
            0
        }).sum + wholeMatchNum
        cache(str) = res
        res
    }
  }
  val res = input.drop(2).map(matchCount).sum

  println(res)
}
