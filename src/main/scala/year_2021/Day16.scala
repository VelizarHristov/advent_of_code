import scala.io.Source

@main
def day16(): Unit = {
  val input = Source.fromFile("resources/16").getLines.next
  val allBits: String = input.flatMap(c => {
    val cInt = Integer.parseInt(c.toString, 16)
    ("000" + cInt.toBinaryString).takeRight(4)
  })

  def binToInt(bin: String) = Integer.parseInt(bin, 2)
  // output is (versionsSum, totalLength)
  def parse(bits: String): (Int, Int) = {
    val version = binToInt(bits.take(3))
    val packetType = binToInt(bits.slice(3, 6))
    if (packetType == 4) {
      var len = 11
      while (bits(len - 5) == '1') {
        len += 5
      }
      (version, len)
    } else {
      var versionSum = version
      if (bits(6) == '0') {
        val subLen = binToInt(bits.slice(7, 22))
        var remainingBits = bits.slice(22, subLen + 22)
        while (remainingBits.nonEmpty) {
          val (nextVersion, nextLen) = parse(remainingBits)
          versionSum += nextVersion
          remainingBits = remainingBits.drop(nextLen)
        }
        (versionSum, subLen + 22)
      } else {
        var subpacketLen = binToInt(bits.slice(7, 18))
        var totalLen = 18
        while (subpacketLen != 0) {
          val (nextVersion, nextLen) = parse(bits.drop(totalLen))
          versionSum += nextVersion
          totalLen += nextLen
          subpacketLen -= 1
        }
        (versionSum, totalLen)
      }
    }
  }

  val (res, _) = parse(allBits)
  println(res)
}
