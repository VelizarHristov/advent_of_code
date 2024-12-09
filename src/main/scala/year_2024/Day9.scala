package year_2024

import io.Source

@main
def day9(): Unit = {
  val input = Source.fromFile("resources/2024/9").getLines.next.map(_.asDigit)
  val disk = new Array[Int](input.sum)
  var lastIdx = 0
  for (id <- 0 until (input.length / 2 + 1)) {
    for (i <- 0 until input(2 * id))
      disk(lastIdx + i) = id
    lastIdx += input(2 * id)
    val nextLen = util.Try(input(2 * id + 1)).getOrElse(0)
    for (i <- 0 until nextLen)
      disk(lastIdx + i) = 0
    lastIdx += nextLen
  }
  def firstFreeIdx = disk.drop(input.head).indexOf(0) + input.head
  def lastNonFreeIdx = disk.lastIndexWhere(_ != 0)
  while (firstFreeIdx < lastNonFreeIdx) {
    val nonFree = lastNonFreeIdx
    disk(firstFreeIdx) = disk(nonFree)
    disk(nonFree) = 0
  }
  val res = disk.zipWithIndex.map((a, i) => (a * i).toLong).sum
  println(res)
}
