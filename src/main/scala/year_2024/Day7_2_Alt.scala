package year_2024

import io.Source
import cats.syntax.traverse.toTraverseOps

@main
def day7_2_Alt(): Unit = {
  val res = Source.fromFile("resources/2024/7").getLines.flatMap(line => {
    val Array(firstStr, rest) = line.split(": ")
    val total = firstStr.toLong
    val nums = rest.split(' ').map(_.toLong).toList
    val hasSolution = nums.tail.traverse(n => LazyList(
      (acc: Long) => acc + n,
      (acc: Long) => acc * n,
      (acc: Long) => (acc.toString + n.toString).toLong
    )).exists(_.foldLeft(nums.head)(
      (acc, op) => op(acc)
    ) == total)
    Option.when(hasSolution)(total)
  }).sum
  println(res)
}
