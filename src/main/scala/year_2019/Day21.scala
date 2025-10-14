package year_2019

@main
def day21(): Unit =
  val program = IntcodeComputer.fromInputs("21")
  val inputProgramStr = (
"""NOT J J
   AND A J
   AND B J
   AND C J
   NOT J J
   AND D J
   WALK
""")
  val inputCode = inputProgramStr.toCharArray.toList.map(_.toLong)
  val res = program.run(inputCode).last
  println(res)
