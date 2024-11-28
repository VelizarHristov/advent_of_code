package year_2021

import io.Source
import scala.collection.mutable

@main
def day16_2(): Unit = {
  val input = Source.fromFile("resources/2021/16").getLines.next
  val allBits: String = input.flatMap(c => {
    val cInt = Integer.parseInt(c.toString, 16)
    ("000" + cInt.toBinaryString).takeRight(4)
  })

  def binToInt(bin: String) = Integer.parseInt(bin, 2)
  // output is (num, totalLength)
  def parse(bits: String): (BigInt, Int) = {
    val packetType = binToInt(bits.slice(3, 6))
    if (packetType == 4) {
      var binStr = bits.slice(7, 11)
      var len = 11
      while (bits(len - 5) == '1') {
        binStr += bits.slice(len + 1, len + 5)
        len += 5
      }
      val num = BigInt(binStr, 2)
      (num, len)
    } else {
      val nums = mutable.Queue[BigInt]()
      val len = if (bits(6) == '0') {
        val subLen = binToInt(bits.slice(7, 22))
        var remainingBits = bits.slice(22, subLen + 22)
        while (remainingBits.nonEmpty) {
          val (num, nextLen) = parse(remainingBits)
          nums += num
          remainingBits = remainingBits.drop(nextLen)
        }
        subLen + 22
      } else {
        var subpacketLen = binToInt(bits.slice(7, 18))
        var totalLen = 18
        while (subpacketLen != 0) {
          val (num, nextLen) = parse(bits.drop(totalLen))
          nums += num
          totalLen += nextLen
          subpacketLen -= 1
        }
        totalLen
      }
      val num = packetType match {
        case 0 => nums.sum
        case 1 => nums.product
        case 2 => nums.min
        case 3 => nums.max
        case 5 => BigInt(if (nums(0) > nums(1)) 1 else 0)
        case 6 => BigInt(if (nums(0) < nums(1)) 1 else 0)
        case 7 => BigInt(if (nums(0) == nums(1)) 1 else 0)
        case _ => throw new IllegalArgumentException("Invalid packet type: " + packetType)
      }
      (num, len)
    }
  }

  val (res, _) = parse(allBits)
  println(res)
}
