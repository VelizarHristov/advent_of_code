package year_2019

import io.Source
import collection.mutable

@main
def day23_2(): Unit =
  case class Program(prog: mutable.Map[BigInt, BigInt],
    inputBuffer: mutable.Queue[BigInt],
    outputBuffer: mutable.ArrayBuffer[BigInt]
  ):
    var pos = BigInt(0)
    var relOffset = BigInt(0)
    def op = prog(pos).toString
    def command = op.takeRight(2).toInt
    def mode(i: Int) =
      val modes = ("0" + op.dropRight(2)).toInt
      (modes / math.pow(10, i)).round % 10
    def param(i: Int): BigInt =
      val immediateParam = prog(pos + i + 1)
      mode(i) match
        case 0 => prog(immediateParam)
        case 1 => immediateParam
        case 2 => prog(immediateParam + relOffset)
    def numInstructions = command match
      case 1 | 2 | 7 | 8 => 4
      case 3 | 4 | 9 => 2
      case 5 | 6 => 3
    def storeNum(v: BigInt) =
      val storeAddr = prog(pos + numInstructions - 1)
      if (mode(numInstructions - 2) == 2)
        prog(storeAddr + relOffset) = v
      else
        prog(storeAddr) = v

    def run() =
      var receivedInput = false
      def input = inputBuffer.removeHeadOption().getOrElse(BigInt(-1))

      while (command != 99 && !receivedInput)
        var toStore: Option[BigInt] = None
        var newPos = pos + numInstructions
        command match
          case 1 => toStore = Some(param(0) + param(1))
          case 2 => toStore = Some(param(0) * param(1))
          case 3 =>
            receivedInput = true
            toStore = Some(input)
          case 4 => outputBuffer += param(0)
          case 5 => if param(0) != 0 then newPos = param(1)
          case 6 => if param(0) == 0 then newPos = param(1)
          case 7 => toStore = if param(0) < param(1) then Some(1) else Some(0)
          case 8 => toStore = if param(0) == param(1) then Some(1) else Some(0)
          case 9 => relOffset += param(0)
        toStore.foreach(storeNum)
        pos = newPos

  val progArray = Source.fromFile("resources/2019/23").getLines.next.split(',').map(BigInt.apply)
  val prog = progArray.indices
    .map(BigInt.apply)
    .zip(progArray)
    .to(mutable.Map)
    .withDefaultValue(BigInt(0))

  val inputBuffers = (0 until 50).map(i => mutable.Queue(BigInt(i))).toArray
  val outputBuffers = Array.fill(50)(mutable.ArrayBuffer[BigInt]())
  val computers = (0 until 50).toArray.map: i =>
    Program(prog.clone(), inputBuffers(i), outputBuffers(i))

  var nat: Option[(BigInt, BigInt)] = None
  var idlesInARow = 0
  var prevNat = (BigInt(-1), BigInt(-1))
  var halt = false
  while !halt do
    outputBuffers.foreach: buf =>
      buf.grouped(3).foreach: out =>
        val Seq(addr, x, y) = out.toSeq
        if addr == 255 then
          nat = Some((x, y))
        else
          inputBuffers(addr.toInt) ++= Seq(x, y)
      buf.clear()
    computers.foreach(_.run())
    if (inputBuffers ++ outputBuffers).forall(_.isEmpty) then
      idlesInARow += 1
      if (idlesInARow > 2) then
        if prevNat == nat.get then
          halt = true
        val (natX, natY) = nat.get
        inputBuffers(0) ++= Seq(natX, natY)
        prevNat = nat.get
    else
      idlesInARow = 0
  println(nat.get)
