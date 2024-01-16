package year_2015

import io.Source

@main
def day6_2(): Unit = {
  enum Mode:
    case On, Off, Toggle
  val intensity = Array.fill(1000)(Array.fill(1000)(0))
  Source.fromFile("resources/2015/6").getLines.foreach(line => {
    val Seq(x1, y1, x2, y2) = "\\d+".r.findAllIn(line).toSeq.map(_.toInt)
    val mode =
      if (line.contains("turn on"))
        Mode.On
      else if (line.contains("turn off"))
        Mode.Off
      else
        Mode.Toggle
    for (x <- x1 to x2; y <- y1 to y2) {
      if (mode == Mode.Toggle)
        intensity(y)(x) += 2
      else if (mode == Mode.On)
        intensity(y)(x) += 1
      else
        intensity(y)(x) = 0 max (intensity(y)(x) - 1)
    }
  })
  println(intensity.flatten.sum)
}
