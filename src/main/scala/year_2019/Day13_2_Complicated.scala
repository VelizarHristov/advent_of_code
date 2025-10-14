package year_2019

import io.Source
import collection.mutable

// Brute-force through the game. This is what I did initially.
@main
def day13_2_Complicated(): Unit =
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
      .map(_.last).maxOption.getOrElse(-1)
    def info = (prog.toList.toString+"|"+pos).hashCode()
    def runOnce(input: Int): Option[Int] =
      var inputConsumed = false
      var commandsSoFar = 0
      // The program might get in an infinite loop, if it does
      //   then count it as a defeat with score of 0
      while (command != 99 && (!inputConsumed || command != 3) && commandsSoFar < 100_000)
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
        commandsSoFar += 1
      if (commandsSoFar >= 100_000) then
        Some(0)
      else if command == 99 then
        Some(score)
      else
        None

  val prog = 
    val arr = Source.fromFile("resources/2019/13").getLines.next.split(',').map(_.toInt)
    arr.padTo(arr.length * 2, 0)
  prog(0) = 2

  // Search from the most promising state:
  //   Gets the optimal result fairly quickly;
  //   Unfortunately, we don't know when it's gotten the optimal result.
  //   It would take ages to terminate.
  //   Early in the program, it gives the real max.
  val visited = mutable.Set[Int]()
  val allStates = mutable.PriorityQueue((Program(prog.clone()), -1))(
    using (p1, p2) => p1._2.compare(p2._2)
  )
  var maxScore = -1
  while (allStates.nonEmpty)
    val (state, _) = allStates.dequeue()
    if util.Random().nextInt(300) == 0 then
      val a = "allStates.size = " + allStates.size
      val b = "Score = " + state.score
      val c = "Max score = " + maxScore
      println((a, b, c))
    val next = for
      toAdd <- List(-1, 0, 1)
      nextState =
        val st = state.copy(prog = state.prog.clone())
        st.pos = state.pos
        st.relOffset = state.relOffset
        st.accOutputs = state.accOutputs
        st
      res = nextState.runOnce(toAdd)
      _ = res.foreach: score =>
        maxScore = maxScore max score
      if res.isEmpty
    yield (nextState, nextState.info)
    for
      (state, info) <- next
      if !visited(info)
    do
      visited.add(info)
      allStates += ((state, state.score))

  // BFS - theoretically faster to finish,
  //   in practice 30 minutes into it it's still very early on
  // var nextStates = List(Program(prog.clone()))
  // var depth = 0
  // while (nextStates.nonEmpty)
  //   val a = "nextStates.size = " + nextStates.size
  //   val b = "Max score = " + maxScore
  //   val c = "Depth = " + depth
  //   println((a, b, c))
  //   nextStates = for
  //     state <- nextStates
  //     toAdd <- List(-1, 0, 1)
  //     nextState =
  //       val st = state.copy(prog = state.prog.clone())
  //       st.pos = state.pos
  //       st.relOffset = state.relOffset
  //       st.accOutputs = state.accOutputs
  //       st
  //     res = nextState.runOnce(toAdd)
  //     _ = res.foreach: score =>
  //       maxScore = maxScore max score
  //     if res.isEmpty
  //     info = nextState.info
  //     if !visited(info)
  //     _ = visited.add(info)
  //   yield nextState
  //   depth += 1

  println(maxScore)
