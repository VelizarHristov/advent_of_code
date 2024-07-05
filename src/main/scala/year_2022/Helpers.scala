package year_2022

object Helpers {
  extension (point: (Int, Int)) {
    def *(n: Int): (Int, Int) = (point._1 * n, point._2 * n)
    def +(point2: (Int, Int)): (Int, Int) = (point._1 + point2._1, point._2 + point2._2)
  }
}
