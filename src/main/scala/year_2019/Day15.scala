package year_2019

import collection.mutable

import helpers.Helpers._

@main
def day15(): Unit =
  val dirs = Seq((0, -1), (0, 1), (-1, 0), (1, 0))
  val program = IntcodeComputer.fromInputs("15")
  var halt = false
  var states = Vector((program, (0, 0)))
  val visited = mutable.Set((0, 0))
  var numMoves = 0
  while !halt do
    states = for
      (state, pos) <- states
      move <- Seq(1, 2, 3, 4)
      newPos = pos + dirs(move - 1)
      if !visited(newPos)
      newState = state.clone
      runRes = newState.runUntil(Seq(move), (_, _, out) => out.length == 1).head
      _ = if runRes == 2 then
        halt = true
      if runRes == 1
      _ = visited.add(newPos)
    yield (newState, newPos)
    numMoves += 1
  println(numMoves)
