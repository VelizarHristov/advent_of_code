package year_2018

import io.Source

@main
def day1_2(): Unit = {
  val input = Source.fromFile("resources/2018/1").getLines.map(_.toInt).toVector
  val sums = collection.mutable.Set(0)
  var sum = 0
  var found: Option[Int] = None
  var i = 0
  while (found.isEmpty) {
    sum += input(i)
    if (sums(sum))
      found = Some(sum)
    sums += sum
    i = (i + 1) % input.length
  }
  println(found.get)
}
