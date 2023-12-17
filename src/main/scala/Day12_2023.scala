import scala.io.Source

@main
def day12_2023(): Unit = {
  val res = Source.fromFile("resources/12_2023").getLines.map(line => {
    val Array(record, arrangementStr) = line.split(' ')
    val arrangement = arrangementStr.split(',').map(_.toInt)
    (0 until record.count(_ == '?')).toSet.subsets.count(subset => {
      var seenSoFar = 0
      val chars = record.map(c => {
        if (c == '?') {
          seenSoFar += 1
          if (subset.contains(seenSoFar - 1)) '#' else '.'
        } else {
          c
        }
      })
      var remainingChars = chars
      remainingChars = chars
      arrangement.forall(arr => {
        remainingChars = remainingChars.dropWhile(_ == '.')
        val assert = remainingChars.takeWhile(_ == '#').length == arr
        remainingChars = remainingChars.drop(arr + 1)
        assert
      }) && chars.count(_ == '#') == arrangement.sum
    })
  }).sum
  println(res)
}
