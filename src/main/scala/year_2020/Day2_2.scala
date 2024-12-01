package year_2020

import io.Source

@main
def day2_2(): Unit = {
  val res = Source.fromFile("resources/2020/2").getLines.count(line => {
    val Array(placesStr, letterStr, password) = line.split(' ')
    val places = placesStr.split('-').map(_.toInt)
    val letter = letterStr.head
    val hits = places.count(i => password(i - 1) == letter)
    hits == 1
  })
  println(res)
}
