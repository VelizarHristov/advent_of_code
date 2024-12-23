package year_2022

import io.Source
import scala.collection.mutable

@main
def day11(): Unit = {
  case class Monkey(operation: Int => Int, testModBy: Int, trueMonkeyId: Int, falseMonkeyId: Int)

  val monkeysList = for (monkeyData <- Source.fromFile("resources/2022/11").getLines.grouped(7).toList) yield {
    val id = monkeyData(0).drop(7).dropRight(1).toInt
    val startingItems = monkeyData(1).drop(18).split(", ").map(_.toInt)
    val operationLine = monkeyData(2).drop(23)
    val operationOp = (x: Int, y: Int) => if (operationLine.contains('+')) x + y else x * y
    val operation = operationLine.drop(2).toIntOption match {
      case Some(num) => operationOp(_, num)
      case None => (x: Int) => operationOp(x, x)
    }
    val testModBy = monkeyData(3).drop(21).toInt
    val trueMonkeyId = monkeyData(4).drop(29).toInt
    val falseMonkeyId = monkeyData(5).drop(30).toInt
    val monkey = Monkey(operation, testModBy, trueMonkeyId, falseMonkeyId)
    (id, monkey, startingItems.to(mutable.ListBuffer))
  }
  val idToMonkey = monkeysList.map { case (id, monkey, _) => (id, monkey) }.toMap
  val idToItems = monkeysList.map { case (id, _, items) => (id, items) }.toMap
  val idToInspects = idToMonkey.keys.map(id => (id, 0)).to(collection.mutable.Map)
  val maxId = idToMonkey.keys.max

  for (_ <- 1 to 20) {
    for (id <- 0 to maxId) {
      val Monkey(operation, testModBy, trueMonkeyId, falseMonkeyId) = idToMonkey(id)
      val items = idToItems(id)
      idToInspects(id) += items.length
      for (item <- items) {
        val newItem = operation(item) / 3
        val nextMonkeyId =
          if (newItem % testModBy == 0)
            trueMonkeyId
          else
            falseMonkeyId
        idToItems(nextMonkeyId).append(newItem)
      }
      items.clear()
    }
  }
  val res = idToInspects.values.toList.sorted.takeRight(2).product
  println(res)
}
