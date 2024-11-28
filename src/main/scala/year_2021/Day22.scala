package year_2021

import scala.io.Source
import collection.mutable

@main
def day22(): Unit = {
  val on = mutable.Set.empty[(Int, Int, Int)]
  Source.fromFile("resources/22").getLines.foreach(line => {
    val isOn = line.startsWith("on")
    val coords = "-?\\d+".r.findAllIn(line).toSeq.map(_.toInt)
    for {
      x <- (-50 max coords(0)) to (coords(1) min 50)
      y <- (-50 max coords(2)) to (coords(3) min 50)
      z <- (-50 max coords(4)) to (coords(5) min 50)
    } {
      if (isOn)
        on.add((x, y, z))
      else
        on.remove((x, y, z))
    }
  })
  println(on.size)
}
