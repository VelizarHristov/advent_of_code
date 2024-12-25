package year_2024

import io.Source
import collection.mutable

@main
def day24(): Unit = {
  val input = Source.fromFile("resources/2024/24").getLines.toVector
  val wires = input.takeWhile(_.nonEmpty).map(line => {
    val Array(name, value) = line.split(": ")
    ((name, value == "1"))
  }).toMap
  val gates = input.drop(wires.size + 1).map(line => {
    val Array(start, name) = line.split(" -> ")
    val gate = List("AND", "XOR", "OR").find(gate =>
      start.contains(gate)
    ).get
    val Array(in1, in2) = start.split(" " + gate + " ")
    (name, (gate, in1, in2))
  }).toMap
  val gateValues = mutable.Map.empty[String, Boolean] ++ wires
  def evalGate(name: String): Boolean = gateValues.get(name) match {
    case Some(v) => v
    case None =>
      val (gate, in1, in2) = gates(name)
      val res = gate match {
        case "AND" => evalGate(in1) && evalGate(in2)
        case "OR"  => evalGate(in1) || evalGate(in2)
        case "XOR" => evalGate(in1) != evalGate(in2)
      }
      gateValues(name) = res
      res
  }
  val binaryRes = gates.map(_._1).filter(_.startsWith("z")).toVector
        .sortBy(x => x).reverse
        .map(name => if (evalGate(name)) "1" else "0").mkString

  val res = java.lang.Long.parseLong(binaryRes, 2)
  println(res)
}
