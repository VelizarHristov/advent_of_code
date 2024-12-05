package year_2024

import io.Source

@main
def day5(): Unit = {
  val input = Source.fromFile("resources/2024/5").getLines.toVector
  val rules = collection.mutable.Map[Int, List[Int]]().withDefaultValue(List.empty)
  input.takeWhile(_.contains('|')).map(line => {
    val Array(a, b) = line.split('|').map(_.toInt)
    rules(a) = b :: rules(a)
  })
  val mids = input.dropWhile(_.contains('|')).tail.flatMap(line => {
    val nums = line.split(',').map(_.toInt)
    val isOrdered = (for (i <- nums.indices) yield {
      rules(nums(i)).filter(nums.contains)
        .forall((i + 1 until nums.length).map(nums).contains)
    }).forall(_ == true)
    Option.when(isOrdered)(nums(nums.length / 2))
  })
  println(mids.sum)
}
