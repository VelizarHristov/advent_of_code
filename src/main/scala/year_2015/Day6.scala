package year_2015

import io.Source

@main
def day6(): Unit = {
  enum Mode:
    case On, Off, Toggle
  val isOn = Array.fill(1000)(Array.fill(1000)(false))
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
        isOn(y)(x) = !isOn(y)(x)
      else
        isOn(y)(x) = mode == Mode.On
    }
  })
  println(isOn.flatten.count(identity))
}
