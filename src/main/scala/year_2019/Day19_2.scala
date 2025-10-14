package year_2019

@main
def day19_2(): Unit =
  val code = IntcodeComputer.fromInputs("19").code
  def hasBeam(x: Int, y: Int) =
    IntcodeComputer(code.clone())
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
