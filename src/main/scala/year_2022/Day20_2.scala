package year_2022

import io.Source
import year_2022.Helpers.wrapMod

@main
def day20_2(): Unit = {
  val idxToPos = collection.mutable.Map[Int, Int]()
  val posToIdx = collection.mutable.Map[Int, Int]()

  val nums = Source.fromFile("resources/2022/20").getLines().toArray.map(_.toLong * 811589153)
  val zeroPos = nums.indexOf(0)
  for ((num, i) <- nums.zipWithIndex if num != 0) {
    val pos = wrapMod(i - zeroPos - 1, nums.length)
    idxToPos(i) = pos
    posToIdx(pos) = i
  }

  for (_ <- 1 to 10) {
    for (idx <- nums.indices if nums(idx) != 0) {
      val num = nums(idx)
      val pos = idxToPos(idx)
      val newPos = wrapMod(pos + num, nums.length - 1)
      val incr = if (newPos > pos) 1 else -1
      for (i <- pos until newPos by incr) {
        val nextIdx = posToIdx(i + incr)
        posToIdx(i) = nextIdx
        idxToPos(nextIdx) = i
      }
      posToIdx(newPos) = idx
      idxToPos(idx) = newPos
    }
  }
  val res = Seq(1000, 2000, 3000).map(i =>
    nums(posToIdx((i % nums.length) - 1))
  ).sum
  println(res)
}
