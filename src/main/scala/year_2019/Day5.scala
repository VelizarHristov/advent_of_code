package year_2019

import io.Source

@main
def day5(): Unit =
  def input = 1

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
      case 1 | 2 => 4
      case 3 | 4 => 2
    def storeNum(v: Int) =
      val storeAddr = prog(pos + numInstructions - 1)
      prog(storeAddr) = v

    while (command != 99)
      var toStore: Option[Int] = None
      command match
        case 1 => toStore = Some(param(0) + param(1))
        case 2 => toStore = Some(param(0) * param(1))
        case 3 => toStore = Some(input)
        case 4 => outputs = outputs :+ param(0)
      val newPos = pos + numInstructions
      toStore.foreach(storeNum)
      pos = newPos
    outputs

  val prog = Source.fromFile("resources/2019/5").getLines.next.split(',').map(_.toInt)
  val output = runProgram(prog)
  if output.init.forall(_ == 0) then
    println(output.last)
  else
    println("Error: output = " + output)
