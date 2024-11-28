import io.Source
import collection.mutable

/**
 * Mostly done by hand, this program just checks the results.
 *
 * 24_edited is a simplified version of 24 which should be the same program
 *
 * The program uses `z` as a stack, storing the numbers in base 26 (instead of the regular bytes which are 256)
 * It adds and removes at least 3 numbers to the stack regardless of its input
 * It may also add up to 7 more numbers depending on the input
 * The only way to have z = 0 is to not add any of the 7 numbers
 * For each of them, it compares two input numbers and if one of them +/- a flat
 *   value is equal to the other then it does NOT add them, otherwise it adds them.
 *   For example if {digit_3} - 1 == {digit_4} then it adds an item to the stack.
 *   Hence, digits 3 and 4 must be one of eight possibilities: 12, 23, 34, .. 89
 *   For part 1, we choose the max by hand, for part 2 we choose the min by hand.
 * In particular, these equalities must hold to have z=0 (for my problem input):
 *   inp#3 - 1 = inp#4
 *   inp#5 + 5 = inp#6
 *   inp#7 + 8 = inp#8
 *   inp#2 + 4 = inp#9
 *   inp#1 + 3 = inp#10
 *   inp#12 - 6 = inp#13
 *   inp#11 + 2 = inp#14
 */
@main
def day24(): Unit = {
  val input = Source.fromFile("resources/24").getLines.toArray

  def check(inputNumber: String): BigInt = {
    val vars = mutable.Map[Char, BigInt]('w' -> 0, 'x' -> 0, 'y' -> 0, 'z' -> 0)
    val number = mutable.Queue[Int]()
    inputNumber.map(_.asDigit).foreach(number.enqueue)
    for (line <- input) {
      val parts = line.split(' ')
      val op = parts.head
      val a = parts(1).head
      lazy val b = parts(2).toIntOption.map(BigInt.apply).getOrElse(vars(parts(2).head))
      op match {
        case "inp" => vars(a) = number.dequeue()
        case "add" => vars(a) += b
        case "mul" => vars(a) *= b
        case "div" => vars(a) /= b
        case "mod" => vars(a) %= b
        case "eql" => vars(a) = if (vars(a) == b) 1 else 0
        case _ => throw new IllegalArgumentException
      }
    }
    vars('z')
  }

  println(check("65984919997939")) // part 1
  println(check("11211619541713")) // part 2
}
