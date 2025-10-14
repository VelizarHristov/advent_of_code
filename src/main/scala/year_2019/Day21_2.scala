package year_2019

@main
def day21_2(): Unit =
  val program = IntcodeComputer.fromInputs("21")
  val inputProgramStr = (
"""NOT J J
   AND A J
   AND B J
   AND C J
   NOT J J
   AND D J
   OR F T
   OR I T
   AND E T
   OR H T
   AND T J
   RUN
""")
  val inputCode = inputProgramStr.toCharArray.toList.map(_.toLong)
  val res = program.run(inputCode).last
  println(res)
