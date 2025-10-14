package year_2019

@main
def day13_2(): Unit =
  val state = IntcodeComputer.fromInputs("13")
  state.code(0) = 2
  var paddleX = -1L
  var ballX = -1L
  var score = -1L

  while (state.command != 99)
    val toAdd = ballX - paddleX match
      case q if q > 0 => 1
      case q if q < 0 => -1
      case 0 => 0
    val out = state.runUntil(
      Seq(toAdd),
      (cmd, inputs, _) => inputs.isEmpty && cmd == 3
    )
    out.grouped(3).foreach: ls =>
      ls match
        case Vector(x, _, 3) => paddleX = x
        case Vector(x, _, 4) => ballX = x
        case Vector(-1, 0, newScore) => score = newScore
        case _ =>
  println(score)
