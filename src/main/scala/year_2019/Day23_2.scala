package year_2019

import collection.immutable

@main
def day23_2(): Unit =
  val program = IntcodeComputer.fromInputs("23")
  val programs = Array.fill(50)(program.clone)
  val buf = (0 until 50).map(i => immutable.Queue(i.toLong)).toArray
  var nat: Option[(Long, Long)] = None
  var idlesInARow = 0
  var prevNat = (-1L, -1L)
  var halt = false
  while !halt do
    (0 until 50).foreach: i =>
      val (next, newBuf) = buf(i).dequeueOption.getOrElse((-1L, buf(i)))
      buf(i) = newBuf
      programs(i).runUntil(
        Seq(next), (_, inp, _) => inp.isEmpty
      ).grouped(3).foreach: out =>
        val Seq(addr, x, y) = out.toSeq
        if addr == 255 then
          nat = Some((x, y))
        else
          buf(addr.toInt) = buf(addr.toInt).enqueueAll(Seq(x, y))

    if (buf).forall(_.isEmpty) then
      idlesInARow += 1
      if (idlesInARow > 2) then
        if prevNat == nat.get then
          halt = true
        val (natX, natY) = nat.get
        buf(0) ++= Seq(natX, natY)
        prevNat = nat.get
    else
      idlesInARow = 0

  val res = nat.get._2
  println(res)
