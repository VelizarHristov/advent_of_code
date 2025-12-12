package year_2025

import io.Source

@main
def day11_2(): Unit =
  val input = Source.fromFile("resources/2025/11").getLines.toArray.map(line => {
    val Array(from, to) = line.split(": ")
    val dsts = to.split(' ')
    (from, dsts)
  }).toMap

  // DFS
  val numPathsMemo = collection.mutable.Map[(String, String), Long]()
  def numPaths(src: String, dst: String): Long = src match
    case "out" => 0
    case s if s == dst => 1
    case _ =>
      lazy val res = input(src).map(s => numPaths(s, dst)).sum
      numPathsMemo.getOrElseUpdate((src, dst), res)
  val (a, b) = if (numPaths("fft", "dac") == 0) ("dac", "fft") else ("fft", "dac")
  val res = numPaths("svr", a) * numPaths(a, b) * numPaths(b, "out")
  println(res)
