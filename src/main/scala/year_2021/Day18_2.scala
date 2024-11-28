package year_2021

import scala.annotation.{tailrec, targetName}
import scala.io.Source

@main
def day18_2(): Unit = {
  trait SnailNum:
    lazy val magnitude: Int
    @targetName("add")
    def +(that: SnailNum): Node = Node(this, that)
  case class Leaf(i: Int) extends SnailNum:
    override val toString: String = i.toString
    lazy val magnitude: Int = i
  case class Node(l: SnailNum, r: SnailNum) extends SnailNum:
    override val toString: String = "[" + l + "," + r + "]"
    lazy val magnitude: Int = 3 * l.magnitude + 2 * r.magnitude

  def parse(s: String): SnailNum = {
    if (s(0).isDigit) {
      Leaf(s(0).asDigit)
    } else {
      val l = parse(s.tail)
      val r = parse(s.drop(l.toString.length + 2))
      Node(l, r)
    }
  }

  def addToLeftmost(node: SnailNum, toAdd: Int): SnailNum = node match {
    case Leaf(i) => Leaf(i + toAdd)
    case Node(l, r) => Node(addToLeftmost(l, toAdd), r)
  }

  def addToRightmost(node: SnailNum, toAdd: Int): SnailNum = node match {
    case Leaf(i) => Leaf(i + toAdd)
    case Node(l, r) => Node(l, addToRightmost(r, toAdd))
  }

  def explode(node: SnailNum, depth: Int = 0): (SnailNum, Option[Int], Option[Int]) = node match {
    case Leaf(_) => (node, None, None)
    case Node(Leaf(l), Leaf(r)) if depth == 4 => (Leaf(0), Some(l), Some(r))
    case Node(l, r) =>
      explode(l, depth + 1) match {
        case (newL, None, None) =>
          val (newR, leftAdd, rightAdd) = explode(r, depth + 1)
          val newNode = Node(addToRightmost(newL, leftAdd.getOrElse(0)), newR)
          (newNode, None, rightAdd)
        case (newL, leftAdd, rightAdd) =>
          val newNode = Node(newL, addToLeftmost(r, rightAdd.getOrElse(0)))
          (newNode, leftAdd, rightAdd.map(_ => 0))
      }
  }

  def split(node: SnailNum): SnailNum = node match {
    case Leaf(i) if i >= 10 => Node(Leaf(i / 2), Leaf(i - i / 2))
    case Leaf(_) => node
    case Node(l, r) =>
      val newL = split(l)
      if (newL == l)
        Node(l, split(r))
      else
        Node(newL, r)
  }

  @tailrec
  def reduce(n: SnailNum): SnailNum = {
    var prev = n
    var n2 = explode(prev)._1
    while (n2 != prev) {
      prev = n2
      n2 = explode(prev)._1
    }
    val n3 = split(n2)
    if (n == n3)
      n
    else
      reduce(n3)
  }

  val input = Source.fromFile("resources/18").getLines.toArray
  val sums = for (s1 <- input; s2 <- input; if s1 != s2) yield {
    val n = parse(s1) + parse(s2)
    reduce(n).magnitude
  }
  val res = sums.max
  println(res)
}
