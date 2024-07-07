package year_2022

import annotation.targetName

object Helpers {
  extension (point: (Int, Int)) {
    @targetName("mult")
    def *(n: Int): (Int, Int) = (point._1 * n, point._2 * n)
    @targetName("add")
    def +(point2: (Int, Int)): (Int, Int) = (point._1 + point2._1, point._2 + point2._2)
  }

  def safeGet[A](ls: Seq[Seq[A]], x: Int, y: Int): Option[A] = {
    ls.lift(x).getOrElse(Seq()).lift(y)
  }
}
