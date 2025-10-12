package year_2019

import io.Source
import collection.mutable

import helpers.Helpers._

@main
def day25(): Unit =
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

    def run(inputStr: String): String =
      val inputs = inputStr.toCharArray().toVector.map(_.toInt)
      var outputs = Vector[Int]()
      var nextInputIdx = 0
      def input =
        val res = inputs(nextInputIdx)
        nextInputIdx += 1
        res
      def output = outputs.map(_.toChar).mkString

      while (command != 99 && !output.endsWith("Command?"))
        var toStore: Option[BigInt] = None
        var newPos = pos + numInstructions
        command match
          case 1 => toStore = Some(param(0) + param(1))
          case 2 => toStore = Some(param(0) * param(1))
          case 3 => toStore = Some(input)
          case 4 => outputs = outputs :+ param(0).toInt
          case 5 => if param(0) != 0 then newPos = param(1)
          case 6 => if param(0) == 0 then newPos = param(1)
          case 7 => toStore = if param(0) < param(1) then Some(1) else Some(0)
          case 8 => toStore = if param(0) == param(1) then Some(1) else Some(0)
          case 9 => relOffset += param(0)
        toStore.foreach(storeNum)
        pos = newPos
      output

  val progArray = Source.fromFile("resources/2019/25").getLines.next.split(',').map(BigInt.apply)
  val prog = progArray.indices
    .map(BigInt.apply)
    .zip(progArray)
    .to(mutable.Map)
    .withDefaultValue(BigInt(0))
  val program = Program(prog)

  // Picking these up only does harmful things
  val itemsToIgnore = Set(
    "giant electromagnet",
    "photons",
    "escape pod",
    "infinite loop",
    "molten lava"
  )
  val reverseDir =
    val map = Map("north" -> "south", "east" -> "west")
    map ++ map.map(_.swap)
  case class Location(name: String, dirs: List[String]):
    def isCheckpoint = name == "Security Checkpoint"

  def parseResponse(resp: String) =
    val lines = resp.split('\n')
    def findList(desc: String) = lines.dropWhile(_ != desc)
        .drop(1)
        .takeWhile(_.startsWith("- "))
        .map(_.drop(2))
        .toList
    val dirs = findList("Doors here lead:")
    val items = findList("Items here:").filterNot(itemsToIgnore.contains)
    val name = lines.find(_.startsWith("== ")).get.drop(3).dropRight(3)
    (Location(name, dirs), items)

  val visited = mutable.Set[String]()
  val path =
    val rootLocation = parseResponse(program.run("\n"))._1
    mutable.Stack(rootLocation)
  val dirsPath = mutable.Stack[String]()
  val allItems = mutable.Set[String]()
  def goBack(): Unit =
    val backDir = reverseDir(dirsPath.pop())
    program.run(backDir + "\n")
    path.pop()

  // Explore everywhere and take all items (ignore harmful "joke" items)
  while
    path.top.dirs.headOption match
      case Some(dir) if !path.top.isCheckpoint =>
        val (next, items) = parseResponse(program.run(dir + "\n"))
        items.foreach: item =>
          program.run(s"take $item\n")
          allItems += item
        dirsPath.push(dir)
        val last = path.pop()
        val lastWithoutDir = last.copy(dirs = last.dirs.filter(_ != dir))
        path.push(lastWithoutDir)
        path.push(next)
        if visited(next.name) then
          goBack()
        visited += next.name
      case _ => goBack()
    dirsPath.nonEmpty
  do ()
  // At this point we are back to the start

  // Go to the security checkpoint
  Seq("south\n", "east\n", "south\n").foreach(program.run)
  // Use brute-force to find which items to keep
  val res = allItems.subsets().flatMap(toDrop => {
    toDrop.foreach(item => program.run(s"drop $item\n"))
    val out = program.run("west\n")
    if out.contains("Alert!") then
      toDrop.foreach(item => program.run(s"take $item\n"))
      None
    else
      Some(out)
  }).next
  println(res)
