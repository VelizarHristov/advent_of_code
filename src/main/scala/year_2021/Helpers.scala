import collection.Seq

def safeGet[A](ls: Seq[Seq[A]], x: Int, y: Int): Option[A] = {
  ls.lift(x).getOrElse(Seq()).lift(y)
}

def safeGet[A](ls: Seq[Seq[A]], x: Int, y: Int, alt: A): A = safeGet(ls, x, y).getOrElse(alt)
