package year_2019

import collection.immutable

@main
def day23(): Unit =
  val program = IntcodeComputer.fromInputs("23")
  val programs = Array.fill(50)(program.clone)
  val buf = (0 until 50).map(i => immutable.Queue(i.toLong)).toArray
  var res: Option[Long] = None
  while res.isEmpty do
    (0 until 50).foreach: i =>
      (1 to 2).foreach: _ =>
        val (next, newBuf) = buf(i).dequeueOption.getOrElse((-1L, buf(i)))
        buf(i) = newBuf
        programs(i).runUntil(
          Seq(next), (_, inp, _) => inp.isEmpty
        ).grouped(3).foreach: out =>
          val Seq(addr, x, y) = out.toSeq
          if addr == 255 then
            res = Some(y)
          else
            buf(addr.toInt) ++= Seq(x, y)
  println(res.get)
