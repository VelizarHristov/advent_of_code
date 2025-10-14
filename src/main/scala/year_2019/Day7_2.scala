package year_2019

import io.Source

@main
def day7_2(): Unit =
  case class Program(prog: Array[Int], var pos: Int = 0):
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

    def run(inputs: Seq[Int]): Vector[Int] =
      var outputs = Vector[Int]()
      var nextInputIdx = 0
      def input =
        val res = inputs(nextInputIdx)
        nextInputIdx += 1
        res

      while (command != 99 && outputs.isEmpty)
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

  val prog = Source.fromFile("resources/2019/7").mkString.split(',').map(_.toInt)
  val res = (5 to 9).permutations.map(seq =>
    val programs = Array.fill(5)(Program(prog.clone()))
    var nextProgIdx = 0
    var lastOutput = 0
    def nextProg = programs(nextProgIdx % 5)

    while (nextProg.command != 99)
      val inputs = if (nextProgIdx < 5) then
        Vector(seq(nextProgIdx), lastOutput)
      else
        Vector(lastOutput)
      for (output <- nextProg.run(inputs).headOption)
        nextProgIdx += 1
        lastOutput = output
    lastOutput
  ).max
  println(res)
