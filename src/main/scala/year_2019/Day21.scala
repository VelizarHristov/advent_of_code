package year_2019

import io.Source

@main
def day21(): Unit =
  case class Program(prog: Array[Int], var pos: Int = 0, var relOffset: Int = 0):
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

    def run(inputs: Seq[Int]): Vector[Int] =
      var outputs = Vector[Int]()
      var nextInputIdx = 0
      def input =
        val res = inputs(nextInputIdx)
        nextInputIdx += 1
        res

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
          case 9 => relOffset += param(0)
        toStore.foreach(storeNum)
        pos = newPos
      outputs

  val prog = 
    val arr = Source.fromFile("resources/2019/21").getLines.next.split(',').map(_.toInt)
    arr.padTo(arr.length * 2, 0)

  val inputProgramStr = (
"""NOT J J
   AND A J
   AND B J
   AND C J
   NOT J J
   AND D J
   WALK
""")
  val inputCode = inputProgramStr.toCharArray.toList.map(_.toInt)
  val res = Program(prog).run(inputCode).last
  println(res)
