package year_2015

import java.security.MessageDigest

@main
def day4_2(): Unit = {
  val md: MessageDigest = MessageDigest.getInstance("MD5")
  def md5Matches(inputStr: String): Boolean = {
    val bytes = md.digest(inputStr.getBytes())
    bytes.startsWith(Array(0, 0, 0))
  }
  val input = "iwrupvqb"
  var i = 0
  while (!md5Matches(input + i))
    i += 1
  println(i)
}
