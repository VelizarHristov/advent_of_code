package year_2019

import io.Source
import collection.mutable.{Queue, ArrayBuffer}

object IntcodeComputer:
  def fromInputs(filename: String) =
    val arr = Source.fromFile(s"resources/2019/$filename").mkString.split(',').map(_.toLong)
    val expanded = arr.padTo(arr.length * 3, 0L)
    IntcodeComputer(expanded)

case class IntcodeComputer(code: Array[Long]):
  var pos = 0
  var relOffset = 0
  def op = code(pos).toString
  def command = op.takeRight(2).toInt
  def mode(i: Int) =
    val modes = ("0" + op.dropRight(2)).toInt
    (modes / math.pow(10, i)).round % 10
  def param(i: Int): Long =
    val immediateParam = code(pos + i + 1)
    mode(i) match
      case 0 => code(immediateParam.toInt)
      case 1 => immediateParam
      case 2 => code(immediateParam.toInt + relOffset)
  def numInstructions = command match
    case 1 | 2 | 7 | 8 => 4
    case 3 | 4 | 9 => 2
    case 5 | 6 => 3
  def storeNum(v: Long) =
    val storeAddr = code(pos + numInstructions - 1).toInt
    if (mode(numInstructions - 2) == 2)
      code(storeAddr + relOffset) = v
    else
      code(storeAddr) = v
  override def clone =
    val that = IntcodeComputer(code.clone())
    that.pos = pos
    that.relOffset = relOffset
    that

  def run(inputs: Seq[Long]): Vector[Long] =
    runUntil(inputs, (_, _, _) => false)

  def runUntil(
    inputs: Seq[Long],
    terminateFunc: (Int, Queue[Long], ArrayBuffer[Long]) => Boolean
  ): Vector[Long] =
    val inputQ = Queue.from(inputs)
    val outputs = ArrayBuffer[Long]()

    while command != 99 && !terminateFunc(command, inputQ, outputs) do
      var toStore: Option[Long] = None
      var newPos = pos + numInstructions
      command match
        case 1 => toStore = Some(param(0) + param(1))
        case 2 => toStore = Some(param(0) * param(1))
        case 3 => toStore = Some(inputQ.dequeue())
        case 4 => outputs += param(0)
        case 5 => if param(0) != 0 then newPos = param(1).toInt
        case 6 => if param(0) == 0 then newPos = param(1).toInt
        case 7 => toStore = if param(0) < param(1) then Some(1) else Some(0)
        case 8 => toStore = if param(0) == param(1) then Some(1) else Some(0)
        case 9 => relOffset += param(0).toInt
      toStore.foreach(storeNum)
      pos = newPos

    outputs.toVector

// Days 2, 5, 7 needed fewer specificationss from the intcode computer
// here I am re-solving them with the complete computer in this file
// check their files to see solutions that implement the partial specifications
@main
def days_2_5_7(): Unit =
  { // day 2, part 1
    val program = IntcodeComputer.fromInputs("2")
    program.code(1) = 12
    program.code(2) = 2
    program.run(Seq())
    val res = program.code(0)
    println("Day 2, part 1: " + res)
  }
  { // day 2, part 2
    val originalProgram = IntcodeComputer.fromInputs("2")
    val target = 19690720
    var res: Option[Long] = None
    for (input1 <- 0 to 100; input2 <- 0 to 100)
      val program = originalProgram.clone
      program.code(1) = input1
      program.code(2) = input2
      program.run(Seq())
      if program.code(0) == target then
        res = Some(100 * input1 + input2)
    println("Day 2, part 2: " + res.get)
  }
  { // day 5, part 1
    val program = IntcodeComputer.fromInputs("5")
    val res = program.run(Seq(1)).last
    println("Day 5, part 1: " + res)
  }
  { // day 5, part 2
    val program = IntcodeComputer.fromInputs("5")
    val res = program.run(Seq(5)).last
    println("Day 5, part 2: " + res)
  }
  { // day 7, part 1
    val program = IntcodeComputer.fromInputs("7")
    val res = (0 to 4).permutations.map(seq =>
      var lastOutput = 0L
      for (next <- seq)
        val inputs = List(next.toLong, lastOutput)
        lastOutput = program.clone.run(inputs).head
      lastOutput
    ).max
    println("Day 7, part 1: " + res)
  }
  { // day 7, part 2
    val prog = IntcodeComputer.fromInputs("7")
    val res = (5 to 9).permutations.map(seq =>
      val programs = Array.fill(5)(prog.clone())
      var nextProgIdx = 0
      var lastOutput = 0L
      def nextProgram = programs(nextProgIdx % 5)

      while (nextProgram.command != 99)
        val inputs = if (nextProgIdx < 5) then
          Vector(seq(nextProgIdx).toLong, lastOutput)
        else
          Vector(lastOutput)
        val outputOpt = nextProgram.runUntil(
          inputs,
          (_, _, out) => out.nonEmpty
        ).headOption
        for (output <- outputOpt)
          nextProgIdx += 1
          lastOutput = output
      lastOutput
    ).max
    println("Day 7, part 2: " + res)
  }
