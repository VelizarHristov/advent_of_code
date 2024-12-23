package year_2024

import io.Source
import collection.mutable

// coded for my specific problem input, doesn't work on the test input
@main
def day17_2(): Unit = {
  val input = Source.fromFile("resources/2024/17").getLines.toArray
  val program = input.last.drop("Program: ".length)
    .split(',').map(_.toInt)
  def runProgram(regA: Long): Vector[Int] = {
    val reg = Array(regA, 0, 0)
    var instrPointer = 0
    val output = mutable.ArrayBuffer[Int]()
    while (instrPointer < program.length) {
      val opcode = program(instrPointer)
      val op = program(instrPointer + 1)
      lazy val comboOp = if (op <= 3) op.toLong else reg(op - 4)
      opcode match {
        case 0 | 6 | 7 => reg(List(0, 6, 7).indexOf(opcode)) = reg(0) / Math.pow(2, comboOp.toDouble).toLong
        case 1 => reg(1) ^= op
        case 2 => reg(1) = comboOp % 8
        case 3 if (reg(0) != 0) => instrPointer = op - 2
        case 4 => reg(1) ^= reg(2)
        case 5 => output += (comboOp % 8).toInt
        case _ =>
      }
      instrPointer += 2
    }
    output.toVector
  }

  /*** How it works:
   * 
   * Last 2 outputs only depend on the last 6 bits - brute-force all solutions for them
   * 
   * The 2 outputs before then only depend on the last 12 bits,
   *   but we have narrowed down what the last 6 bits to a few options,
   *   so brute-force the remaining 6 bits to get all 12 bits that
   *   produce the last 4 outputs
   * 
   * Repeat this, adding 2 bits at a time, each time the list of possibilities
   *   has up to than 10 elements.
   */
  var solutions = Seq(0l)
  def matchedOutput: Int = {
    val bitsSoFar = 64 - java.lang.Long.numberOfLeadingZeros(solutions.head)
    (bitsSoFar / 3.0).ceil.toInt
  }
  while (matchedOutput < program.size) {
    val targetNext = program.reverse.take(2 + matchedOutput).reverse.toVector
    solutions = (for {
      prevBits <- solutions
      nextBits <- 0l to Math.pow(8, 2).toLong - 1
      next = (prevBits << 6) + nextBits
      if runProgram(next) == targetNext
    } yield next).distinct
  }

  println(solutions.head)
}
