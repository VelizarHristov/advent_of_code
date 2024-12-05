package year_2024

import io.Source

@main
def day5_2(): Unit = {
  val input = Source.fromFile("resources/2024/5").getLines.toVector
  val rules = collection.mutable.Map[Int, List[Int]]().withDefaultValue(List.empty)
  input.takeWhile(_.contains('|')).map(line => {
    val Array(a, b) = line.split('|').map(_.toInt)
    rules(a) = b :: rules(a)
  })
  val mids = input.dropWhile(_.contains('|')).tail.map(line => {
    val nums = line.split(',').map(_.toInt)
    val isOrdered = (for (i <- nums.indices) yield {
      rules(nums(i)).filter(nums.contains)
        .forall((i + 1 until nums.length).map(nums).contains)
    }).forall(_ == true)
    if (isOrdered) {
      0
    } else {
      var numsSoFar = Vector[Int]()
      var remainingNums = nums
      while (remainingNums.nonEmpty) {
        val i = remainingNums.indices.find(i => {
          rules(remainingNums(i)).filter(nums.contains).forall(numsSoFar.contains)
        }).get
        val n = remainingNums(i)
        numsSoFar = numsSoFar.appended(n)
        remainingNums = remainingNums.filter(_ != n)
      }
      numsSoFar(numsSoFar.length / 2)
    }
  })
  println(mids.sum)
}
