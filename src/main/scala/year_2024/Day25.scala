package year_2024

import io.Source

@main
def day25(): Unit = {
  def getHeight(input: Seq[Seq[String]], sym: Char) = input.map(lines => {
    (0 until lines.head.size).map(pinNum => {
      (0 until 7).indexWhere(size => lines(size)(pinNum) == sym) - 1
    })
  })
  val (locksInput, keysInput) = Source.fromFile("resources/2024/25").getLines
    .grouped(8).toVector.partition(_.head.forall(_ == '#'))
  val locks = getHeight(locksInput, '.')
  val keys = getHeight(keysInput, '#')
  val res = keys.map(key => {
    locks.count(lock => {
      key.zip(lock).forall((k, l) => k >= l)
    })
  }).sum

  println(res)
}
