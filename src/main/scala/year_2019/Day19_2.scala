package year_2019

import io.Source
import collection.mutable

@main
def day19_2(): Unit =
  case class Program(prog: mutable.Map[BigInt, BigInt]):
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

    def run(inputs: Seq[BigInt]): Vector[BigInt] =
      var outputs = Vector[BigInt]()
      var nextInputIdx = 0
      def input =
        val res = inputs(nextInputIdx)
        nextInputIdx += 1
        res

      while (command != 99)
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

  val progArray = Source.fromFile("resources/2019/19").getLines.next.split(',').map(BigInt.apply)
  val prog = progArray.indices
    .map(BigInt.apply)
    .zip(progArray)
    .to(mutable.Map)
    .withDefaultValue(BigInt(0))

  def hasBeam(x: Int, y: Int) =
    Program(prog.clone())
      .run(Vector(x, y))
      .head == 1

  // some of below is hardcoded for my data, but easy to adjust
  val MARGIN = 2
  var y = 4
  var minX = 5
  var maxX = 5
  var res: Option[Int] = None
  while res.isEmpty do
    y += 1
    minX = (minX to minX + MARGIN).find(hasBeam(_, y)).get
    maxX = (maxX + MARGIN to maxX by -1).find(hasBeam(_, y)).get
    if hasBeam(maxX - 99, y + 99) then
      res = Some((maxX - 99) * 10000 + y)
  println(res.get)
