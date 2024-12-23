package year_2024

import io.Source
import collection.mutable

@main
def day17(): Unit = {
  val input = Source.fromFile("resources/2024/17").getLines.toArray
  val reg = input.take(3).map(_.drop("Register N: ".length).toInt)
  val program = input.last.drop("Program: ".length)
    .split(',').map(_.toInt)
  var instrPointer = 0
  val output = mutable.ArrayBuffer[Int]()
  while (instrPointer < program.length) {
    val opcode = program(instrPointer)
    val op = program(instrPointer + 1)
    lazy val comboOp = if (op <= 3) op else reg(op - 4)
    opcode match {
      case 0 | 6 | 7 => reg(List(0, 6, 7).indexOf(opcode)) = reg(0) / Math.pow(2, comboOp).toInt
      case 1 => reg(1) ^= op
      case 2 => reg(1) = comboOp % 8
      case 3 if (reg(0) != 0) => instrPointer = op - 2
      case 4 => reg(1) ^= reg(2)
      case 5 => output += comboOp % 8
      case _ =>
    }
    instrPointer += 2
  }

  // println(reg.mkString(" | "))
  println(output.mkString(","))
}
