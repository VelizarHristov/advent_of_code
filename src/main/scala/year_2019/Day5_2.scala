package year_2019

import io.Source

@main
def day5_2(): Unit =
  def input = 5

  def runProgram(prog: Array[Int]): Vector[Int] =
    var pos = 0
    var outputs = Vector[Int]()

    def op = prog(pos).toString
    def command = op.takeRight(2).toInt
    def modes = ("0" + op.dropRight(2)).toInt
    def param(i: Int): Int =
      val mode = (modes / math.pow(10, i)).toInt % 10
      val immediateParam = prog(pos + i + 1)
      mode match
        case 0 => prog(immediateParam)
        case 1 => immediateParam
    def numInstructions = command match
      case 1 | 2 | 7 | 8 => 4
      case 3 | 4 => 2
      case 5 | 6 => 3
    def storeNum(v: Int) =
      val storeAddr = prog(pos + numInstructions - 1)
      prog(storeAddr) = v

    while (command != 99)
      var toStore: Option[Int] = None
      var newPos = pos + numInstructions
      command match
        case 1 => toStore = Some(param(0) + param(1))
        case 2 => toStore = Some(param(0) * param(1))
        case 3 => toStore = Some(input)
        case 4 => outputs = outputs :+ param(0)
        case 5 => if param(0) != 0 then newPos = param(1)
        case 6 => if param(0) == 0 then newPos = param(1)
        case 7 => toStore = if param(0) < param(1) then Some(1) else Some(0)
        case 8 => toStore = if param(0) == param(1) then Some(1) else Some(0)
      toStore.foreach(storeNum)
      pos = newPos
    outputs

  val prog = Source.fromFile("resources/2019/5").mkString.split(',').map(_.toInt)
  val output = runProgram(prog)
  if output.init.forall(_ == 0) then
    println(output.last)
  else
    println("Error: output = " + output)
