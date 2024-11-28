import scala.io.Source

@main
def day4_2(): Unit = {
  val input = Source.fromFile("resources/4").getLines.toArray
  val numbers = input.head.split(',').map(_.toInt)
  val boards = input.tail.grouped(6).map(lines => {
    lines.tail.map(_.trim.split("\\s+").map(_.toInt))
  }).toArray

  def hasBingo(board: Array[Array[Int]], marked: Set[Int]): Boolean =
    Seq(board, board.transpose).exists(b => {
      b.exists(_.forall(marked.contains))
    })
  val idx = numbers.indices.indexWhere(idx => {
    val marked = numbers.take(idx).toSet
    boards.forall(hasBingo(_, marked))
  })
  val marked = numbers.take(idx - 1).toSet
  val unmarked = boards.find(!hasBingo(_, marked)).get.flatten.filterNot(marked.contains)

  val res = numbers(idx - 1) * (unmarked.sum - numbers(idx - 1))
  println(res)
}
