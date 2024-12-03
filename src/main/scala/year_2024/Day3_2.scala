package year_2024

import io.Source

@main
def day3_2(): Unit = {
  var isEnabled = true
  var rem = Source.fromFile("resources/2024/3").mkString
  var sum = 0
  while (rem.indexOf("mul(") != -1) {
    var i = rem.indexOf("mul(")
    val j = rem.indexOf("don't()")
    val k = rem.indexOf("do()")
    val next = Seq(i, j, k).filter(_ != -1).min
    if (next != i) {
      isEnabled = next == k
      rem = rem.drop(next + 4)
    } else {
      i += 4
      // would probably be much simpler with regex
      var num1Str = ""
      while (rem(i).isDigit) {
        num1Str += rem(i).toString
        i += 1
      }
      if (num1Str.nonEmpty && rem(i) == ',') {
        i += 1
        var num2Str = ""
        while (rem(i).isDigit) {
          num2Str += rem(i).toString
          i += 1
        }
        if (rem(i) == ')') {
          i += 1
          if (isEnabled)
            sum += num1Str.toInt * num2Str.toInt
        }
      }
      rem = rem.drop(i)
    }
  }
  println(sum)
}
