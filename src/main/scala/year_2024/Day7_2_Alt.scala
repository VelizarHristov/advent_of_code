package year_2024

import io.Source
import cats.syntax.traverse.toTraverseOps

@main
def day7_2_Alt(): Unit = {
  val res = Source.fromFile("resources/2024/7").getLines.flatMap(line => {
    val Array(firstStr, rest) = line.split(": ")
    val total = firstStr.toLong
    val nums = rest.split(' ').map(_.toLong).toList
    // maybe it can be simpler using a different argument for traverse
    val hasSolution = List.fill(nums.size - 1)(LazyList(
      (acc: Long, n: Long) => acc + n,
      (acc: Long, n: Long) => acc * n,
      (acc: Long, n: Long) => (acc.toString + n.toString).toLong
    )).traverse(identity).exists(operations => {
      nums.tail.zip(operations).foldLeft(nums.head) {
        case(acc, (next, op)) => op(acc, next)
      } == total
    })
    Option.when(hasSolution)(total)
  }).sum
  println(res)
}
