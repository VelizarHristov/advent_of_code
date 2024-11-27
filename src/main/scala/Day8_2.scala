import scala.io.Source

@main
def day8_2(): Unit = {
  val res = Source.fromFile("resources/8").getLines.map(line => {
    val (inputs, outputs) = "[a-g]+".r.findAllIn(line).toSeq.map(_.toSet).splitAt(10)
    val one = inputs.find(_.size == 2).get
    val four = inputs.find(_.size == 4).get
    val seven = inputs.find(_.size == 3).get
    val eight = inputs.find(_.size == 7).get
    val (three_, fiveAndTwo) = inputs.filter(_.size == 5).partition(one.subsetOf)
    val three = three_.head
    val (five_, two_) = fiveAndTwo.partition((four diff one).subsetOf)
    val five = five_.head
    val two = two_.head
    val (nineAndSix, zero_) = inputs.filter(_.size == 6).partition(five.subsetOf)
    val zero = zero_.head
    val (nine_, six_) = nineAndSix.partition(four.subsetOf)
    val six = six_.head
    val nine = nine_.head
    val mapping = Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4,
      five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)
    outputs.zipWithIndex.map((sym, idx) => mapping(sym) * Math.pow(10, 3 - idx).toInt).sum
  }).sum
  println(res)
}
