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
  val numPathsMemo = collection.mutable.Map[String, (Long, Boolean, Boolean)]()
  def numPaths(src: String): (Long, Boolean, Boolean) =
    if (src == "out") then
      (1, false, false)
    else
      lazy val res =
        val next = input(src).map(numPaths)
        val hasFft = next.exists(_._2)
        val hasDac = next.exists(_._3)
        val totalPaths = next.filter((_, fft, dac) =>
          (!hasFft || fft) && (!hasDac || dac) 
        ).map(_._1).sum
        (totalPaths, hasFft || src == "fft", hasDac || src == "dac")
      numPathsMemo.getOrElseUpdate(src, res)
  val res = numPaths("svr")._1
  println(res)
