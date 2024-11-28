package year_2021

import scala.io.Source

@main
def day6(): Unit = {
  val fish = Source.fromFile("resources/2021/6").getLines.next.split(",").map(_.toInt).toBuffer
  for (_ <- 1 to 80) {
    for (i <- fish.indices) {
      if (fish(i) == 0) {
        fish(i) = 6
        fish.append(8)
      } else {
        fish(i) -= 1
      }
    }
  }
  println(fish.size)
}
