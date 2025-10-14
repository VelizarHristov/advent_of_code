package year_2019

import collection.mutable

import helpers.Helpers._

@main
def day15_2(): Unit =
  val dirs = Seq((0, -1), (0, 1), (-1, 0), (1, 0))
  val program = IntcodeComputer.fromInputs("15")
  var oxygenState: Option[(IntcodeComputer, (Int, Int))] = None
  var states = Vector((program, (0, 0)))
  var visited = mutable.Set((0, 0))
  var numMoves = 0
  def nextStates =
    numMoves += 1
    for
      (state, pos) <- states
      move <- Seq(1, 2, 3, 4)
      newPos = pos + dirs(move - 1)
      if !visited(newPos)
      newState = state.clone
      runRes = newState.runUntil(Seq(move), (_, _, out) => out.length == 1).head
      _ = if runRes == 2 then
        oxygenState = Some((newState, newPos))
      if runRes == 1
      _ = visited.add(newPos)
    yield (newState, newPos)

  while oxygenState.isEmpty do
    states = nextStates
  states = Vector(oxygenState.get)
  visited = mutable.Set(oxygenState.get._2)
  // last move generates no states, so it does not count
  numMoves = -1
  while states.nonEmpty do
    states = nextStates
  println(numMoves)
