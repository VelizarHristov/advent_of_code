package year_2022

import io.Source

@main
def day13_2(): Unit = {
  case class PacketData(data: List[PacketData | Int]) {
    override def toString: String = "[" + data.mkString(",") + "]"
  }
  def compareTo(data1: PacketData | Int, data2: PacketData | Int): Int = (data1, data2) match {
    case (i1: Int, i2: Int) => i1.compareTo(i2)
    case (PacketData(_), i2: Int) => compareTo(data1, PacketData(List(i2)))
    case (i1: Int, PacketData(_)) => compareTo(PacketData(List(i1)), data2)
    case (PacketData(ls1), PacketData(ls2)) =>
      ls1.zip(ls2).map { case (i1, i2) => compareTo(i1, i2) }
        .find(_ != 0)
        .getOrElse(ls1.length.compareTo(ls2.length))
  }

  def parsePacketData(input: String): (PacketData, String) = {
    if (input.startsWith("[]")) {
      (PacketData(List()), input.drop(2))
    } else {
      val (res, leftover) =
        if (input(1).isDigit) {
          val (digits, rest) = input.tail.span(_.isDigit)
          (digits.toInt, rest)
        } else {
          parsePacketData(input.tail)
        }
      val (PacketData(ls), rest) = parsePacketData("[" + leftover.dropWhile(_ == ','))
      (PacketData(res :: ls), rest)
    }
  }

  val toAdd = Seq("[[2]]", "[[6]]")
  val inputData = Source.fromFile("resources/2022/13").getLines.toList
    .filter(_.nonEmpty)
    .appendedAll(toAdd)
  val allPacketData = inputData.map(x => parsePacketData(x)._1)
  val sorted = allPacketData.sorted(Ordering(compareTo(_, _)))
  val res = sorted.zipWithIndex
    .filter { case (data, _) => toAdd.contains(data.toString) }
    .map { case (data, idx) => idx + 1 }
    .product
  println(res)
}
