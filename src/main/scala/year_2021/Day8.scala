import scala.io.Source

@main
def day8(): Unit = {
  val unambiguousLengths = Set(2, 3, 4, 7)
  val res = Source.fromFile("resources/8").getLines.map(line => {
    line.split("\\| ")(1).split(" ").count(sym => unambiguousLengths.contains(sym.length))
  }).sum
  println(res)
}
