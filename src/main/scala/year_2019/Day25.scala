package year_2019

import collection.mutable

@main
def day25(): Unit =
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

  val program = IntcodeComputer.fromInputs("25")
  def run(in: String): String =
    def parseOut(out: Seq[Long]) = out.map(_.toChar).mkString
    val fullInput = (in + "\n").toCharArray().toVector.map(_.toLong)
    val res = program.runUntil(fullInput, (_, _, out) =>
      parseOut(out.toSeq).endsWith("Command?")
    )
    parseOut(res)

  def runAndParseResponse(input: String) =
    val lines = run(input).split('\n')
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
    val rootLocation = runAndParseResponse("")._1
    mutable.Stack(rootLocation)
  val dirsPath = mutable.Stack[String]()
  val allItems = mutable.Set[String]()
  def goBack(): Unit =
    val backDir = reverseDir(dirsPath.pop())
    run(backDir)
    path.pop()

  // Explore everywhere and take all items (ignore harmful "joke" items)
  while
    path.top.dirs.headOption match
      case Some(dir) if !path.top.isCheckpoint =>
        val (next, items) = runAndParseResponse(dir)
        items.foreach: item =>
          run(s"take $item")
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
  Seq("south", "east", "south").foreach(run)
  // Use brute-force to find which items to keep
  val res = allItems.subsets().flatMap(toDrop => {
    toDrop.foreach(item => run(s"drop $item"))
    val out = run("west")
    if out.contains("Alert!") then
      toDrop.foreach(item => run(s"take $item"))
      None
    else
      Some(out)
  }).next
  println(res)
