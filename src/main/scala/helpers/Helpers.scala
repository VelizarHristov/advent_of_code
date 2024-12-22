package helpers

import annotation.targetName
import scala.reflect.ClassTag

object Helpers {
  extension (point: (Int, Int)) {
    @targetName("mult")
    def *(n: Int): (Int, Int) = (point._1 * n, point._2 * n)
    @targetName("add")
    def +(point2: (Int, Int)): (Int, Int) = (point._1 + point2._1, point._2 + point2._2)
    @targetName("neg")
    def unary_- : (Int, Int) = point * -1
    @targetName("sub")
    def -(point2: (Int, Int)): (Int, Int) = point + -point2
  }

  extension (point: (Int, Int, Int)) {
    @targetName("add")
    def +(point2: (Int, Int, Int)): (Int, Int, Int) = (point._1 + point2._1, point._2 + point2._2, point._3 + point2._3)
  }

  def safeGet[A](ls: Seq[Seq[A]], x: Int, y: Int): Option[A] = {
    ls.lift(x).getOrElse(Seq()).lift(y)
  }

  def safeGet[A: ClassTag](ls: Array[Array[A]], x: Int, y: Int): Option[A] = {
    ls.lift(x).getOrElse(Array()).lift(y)
  }

  def wrapMod(i: Long, j: Int): Int = (((i % j) + j) % j).toInt
  def wrapMod(i: Int, j: Int): Int = wrapMod(i.toLong, j)
}
