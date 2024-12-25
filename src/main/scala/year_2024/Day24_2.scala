package year_2024

import io.Source
import collection.mutable

// Cannot handle a bunch of possible swaps:
//   every time findGate is used (not through findXYGate)
// The one such case in my data is hardcoded.
// Originally, the program crashes in this case and then it is
//   very easy to find manually find what to swap (by inspecting variables).
// That is how I found my swaps.
@main
def day24_2(): Unit = {
  val gates = Source.fromFile("resources/2024/24").getLines
  .dropWhile(_.nonEmpty).drop(1).map(line => {
    val Array(start, name) = line.split(" -> ")
    val gate = List("AND", "XOR", "OR").find(gate =>
      start.contains(gate)
    ).get
    val Array(in1, in2) = start.split(" " + gate + " ")
    (name, (gate, in1, in2))
  }).to(mutable.Map)
  // assumes that there's exactly 1 such output (with different data it can be less or more)
  def findGate(gate: String, in1: String, in2: String): String = {
    gates.find{ case (_, (gateThat, in1That, in2That)) =>
      Set(in1, in2) == Set(in1That, in2That) && gate == gateThat
    }.get._1
  }
  def findXYGate(gate: String, num: String) = findGate(gate, "x" + num, "y" + num)

  val changed = mutable.ArrayBuffer.empty[String]
  def swap(out1: String, out2: String) = {
    changed ++= Seq(out1, out2)
    val swp = gates(out1)
    gates(out1) = gates(out2)
    gates(out2) = swp
  }
  if (findXYGate("XOR", "00") != "z00")
    swap(findXYGate("XOR", "00"), "z00")
  var prevCarryover = findXYGate("AND", "00")
  val lastGate = gates.map(_._1).filter(_.startsWith("z")).map(_.drop(1).toInt).max - 1
  swap("kfp", "hbs") // hardcoded for our data
  var i = 1
  while (i <= lastGate) {
    val iStr = (if (i < 10) "0" else "") + i
    val xorGate = findXYGate("XOR", iStr)
    val andGate = findXYGate("AND", iStr)
    val zGate = findGate("XOR", prevCarryover, xorGate)
    if (zGate != "z" + iStr) {
      swap(zGate, "z" + iStr) // restart
    } else {
      val carryoverWithPrev = findGate("AND", prevCarryover, xorGate)
      prevCarryover = findGate("OR", andGate, carryoverWithPrev)
      i += 1
    }
  }
  if (prevCarryover != "z" + (lastGate + 1))
    swap(prevCarryover, "z" + (lastGate + 1))

  val res = changed.sorted.mkString(",")
  println(res)
}
