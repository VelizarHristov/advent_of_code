package year_2019

import io.Source
import collection.mutable
import helpers.Helpers.wrapMod
import helpers.Helpers._

@main
def day11(): Unit =
  case class Program(
    prog: mutable.Map[BigInt, BigInt],
    var pos: BigInt = 0,
    var relOffset: BigInt = 0,
  ):
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

    def run(inputs: Seq[BigInt]): Vector[BigInt] =
      var outputs = Vector[BigInt]()
      var nextInputIdx = 0
      def input =
        val res = inputs(nextInputIdx)
        nextInputIdx += 1
        res

      while (command != 99 && outputs.length < 2)
        var toStore: Option[BigInt] = None
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

  val progArray = Source.fromFile("resources/2019/11").getLines.next.split(',').map(BigInt.apply)
  val prog = progArray.indices
    .map(BigInt.apply)
    .zip(progArray)
    .to(mutable.Map)
    .withDefaultValue(BigInt(0))
  val program = Program(prog)
  var curPos = (0, 0)
  val dirs = Seq((0, -1), (1, 0), (0, 1), (-1, 0))
  var curDirIdx = 0
  def dir = dirs(curDirIdx)
  var painted = Set[(Int, Int)]()
  var whiteTiles = Set[(Int, Int)]()

  while (program.command != 99)
    val inputs = Seq(if whiteTiles.contains(curPos) then BigInt(1) else BigInt(0))
    program.run(inputs) match
      case Vector(newColor, direction) =>
        if newColor == 0 then
          whiteTiles -= curPos
        else
          whiteTiles += curPos
        painted += curPos
        val dirChange = if direction == 1 then 1 else -1
        curDirIdx = wrapMod(curDirIdx + dirChange.toInt, 4)
        curPos += dir
      case _ =>
  println(painted.size)
