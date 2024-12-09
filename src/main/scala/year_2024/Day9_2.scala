package year_2024

import io.Source

@main
def day9_2(): Unit = {
  trait DiskSpace {
    val size: Int
  }
  case class Free(size: Int) extends DiskSpace
  case class File(size: Int, id: Int) extends DiskSpace
  val input = Source.fromFile("resources/2024/9").getLines.next.map(_.asDigit)
  var disk = Vector[DiskSpace]()
  for (id <- 0 until (input.length / 2 + 1)) {
    disk = disk.appended(File(input(2 * id), id))
    for (len <- util.Try(input(2 * id + 1)))
      if (len != 0)
        disk = disk.appended(Free(len))
  }
  val files = disk.collect{ case f: File => f }.reverse
  for (file@File(size, id) <- files) {
    val spaceIdx = disk.indexWhere {
      case Free(s) if s >= size => true
      case _                    => false
    }
    val fileIdx = disk.indexOf(file)
    if (spaceIdx != -1 && spaceIdx < fileIdx) {
      val size = disk(spaceIdx).size
      disk = disk.updated(fileIdx, Free(size))
      if (size == file.size) {
        disk = disk.updated(spaceIdx, file)
      } else {
        val newSpace = Free(size - file.size)
        disk = disk.take(spaceIdx) ++
          Vector(file, newSpace) ++
          disk.drop(spaceIdx + 1)
      }
    }
  }
  val res = disk.flatMap {
    case Free(size) => Vector.fill(size)(0)
    case File(size, id) => Vector.fill(size)(id)
  }.zipWithIndex.map((id, i) => (id * i).toLong).sum
  println(res)
}
