package year_2021

import scala.io.Source

@main
def day3(): Unit = {
  val input = Source.fromFile("resources/3").getLines.toList
  val zeroCounts = input.transpose.map(_.count(_ == '0'))
  val gammaRate = zeroCounts.map(c => if (c > input.length / 2) '0' else '1').mkString
  val epsilonRate = gammaRate.map(c => if (c == '0') '1' else '0')
  val res = Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)
  println(res)
}
