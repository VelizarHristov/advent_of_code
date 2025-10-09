package year_2019

import io.Source

@main
def day13_2(): Unit =
  case class Program(prog: Array[Int]):
    var pos = 0
    var relOffset = 0
    def op = prog(pos).toString
    def command = op.takeRight(2).toInt
    def mode(i: Int) =
      val modes = ("0" + op.dropRight(2)).toInt
      (modes / math.pow(10, i)).round % 10
    def param(i: Int): Int =
      val immediateParam = prog(pos + i + 1)
      mode(i) match
        case 0 => prog(immediateParam)
        case 1 => immediateParam
        case 2 => prog(immediateParam + relOffset)
    def numInstructions = command match
      case 1 | 2 | 7 | 8 => 4
      case 3 | 4 | 9 => 2
      case 5 | 6 => 3
    def storeNum(v: Int) =
      val storeAddr = prog(pos + numInstructions - 1)
      if (mode(numInstructions - 2) == 2)
        prog(storeAddr + relOffset) = v
      else
        prog(storeAddr) = v

    var accOutputs = Vector[Int]()
    def score = accOutputs.grouped(3)
      .filter(ls => ls.startsWith(Seq(-1, 0)))
      .map(_.last).max
    def posInfo: (Int, Int) =
      var paddleX = -1
      var ballX = -1
      accOutputs.grouped(3).foreach: ls =>
        ls match
          case Vector(x, _, 3) => paddleX = x
          case Vector(x, _, 4) => ballX = x
          case _ =>
      (paddleX, ballX)
    def runOnce(input: Int) =
      var inputConsumed = false
      while (command != 99 && (!inputConsumed || command != 3))
        var toStore: Option[Int] = None
        var newPos = pos + numInstructions
        command match
          case 1 => toStore = Some(param(0) + param(1))
          case 2 => toStore = Some(param(0) * param(1))
          case 3 =>
            toStore = Some(input)
            inputConsumed = true
          case 4 => accOutputs = accOutputs :+ param(0)
          case 5 => if param(0) != 0 then newPos = param(1)
          case 6 => if param(0) == 0 then newPos = param(1)
          case 7 => toStore = if param(0) < param(1) then Some(1) else Some(0)
          case 8 => toStore = if param(0) == param(1) then Some(1) else Some(0)
          case 9 => relOffset += param(0)
        toStore.foreach(storeNum)
        pos = newPos

  val prog = 
    val arr = Source.fromFile("resources/2019/13").getLines.next.split(',').map(_.toInt)
    arr.padTo(arr.length * 2, 0)
  prog(0) = 2

  val state = Program(prog)
  while (state.command != 99)
    val (px, bx) = state.posInfo
    val toAdd = bx - px match
      case q if q > 0 => 1
      case q if q < 0 => -1
      case 0 => 0
    state.runOnce(toAdd)
  println(state.score)
