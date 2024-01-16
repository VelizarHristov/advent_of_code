package year_2015

import collection.mutable
import io.Source

@main
def day7(): Unit = {
  val signal = mutable.Map[String, Int]()
  def getSignal(str: String): Option[Int] = str.toIntOption.orElse(signal.get(str))
  val remaining = mutable.Set[(String, Array[String])]()
  Source.fromFile("resources/2015/7").getLines.foreach(line => {
    val Array(value, key) = line.split(" -> ")
    remaining += ((key, value.split(" ")))
  })

  while (remaining.nonEmpty) {
    for (kv@(key, value) <- remaining.toVector) {
      if (value.length == 1) {
        getSignal(value.head).foreach(arg => {
          signal(key) = arg
          remaining -= kv
        })
      } else if (value(0) == "NOT") {
        getSignal(value(1)).foreach(arg => {
          signal(key) = ~arg
          remaining -= kv
        })
      } else {
        for (arg1 <- getSignal(value(0)); arg2 <- getSignal(value(2))) {
          signal(key) = value(1) match {
            case "AND" => arg1 & arg2
            case "OR" => arg1 | arg2
            case "LSHIFT" => arg1 << arg2
            case "RSHIFT" => arg1 >> arg2
          }
          remaining -= kv
        }
      }
    }
  }
  println(signal("a"))
}
