package year_2022

import io.Source

@main
def day13(): Unit = {
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

  // mutable version
//  def parsePacketData2(input: String): (PacketData, String) = {
//    var str = input.tail
//    val ls = collection.mutable.ListBuffer[PacketData | Int]()
//    while (str.head != ']') {
//      str.head match {
//        case ',' => str = str.tail
//        case '[' =>
//          val (item, remainder) = parsePacketData2(str)
//          ls += item
//          str = remainder
//        case _ =>
//          ls += str.takeWhile(_.isDigit).toInt
//          str = str.dropWhile(_.isDigit)
//      }
//    }
//    (PacketData(ls.toList), str.tail)
//  }

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

  val inputData = Source.fromFile("resources/2022/13").getLines.toList.appended("").grouped(3)
  val indicesInRightOrder = for ((signals, idx) <- inputData.zipWithIndex) yield {
    val Seq(s1, s2) = signals.take(2).map(x => parsePacketData(x)._1)
    if (compareTo(s1, s2) == -1)
      Some(idx + 1)
    else
      None
  }

  println(indicesInRightOrder.flatten.sum)
}
