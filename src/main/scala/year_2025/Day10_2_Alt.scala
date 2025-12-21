package year_2025

import io.Source

import cats.syntax.traverse.toTraverseOps

/** Uses Gaussian elimination then brute-force
  * 
  * Gaussian elimination reduces our problem to a few free variables (ints)
  * Then in order to avoid solutions that are non-integer, fractional,
  *   and to find the optimal one, we just brute-force check all possibilities.
  * For our dataset that's 0-3 variables that go up to 300, so O(300^3 * 3 * 10 * n)
  */
@main
def day10_2_Alt(): Unit =
  val res = Source.fromFile("resources/2025/10").getLines.map(line => {
    val sections = line.split(' ').tail
    val req = sections.last.tail.init.split(',').map(_.toInt)
    val maxPresses = req.max
    val btns = sections.init.map(_.tail.init.split(',').map(_.toInt))
    val reqToBtns = req.indices.toArray.map: i =>
      btns.indices.toArray.map: j =>
        if btns(j).contains(i) then 1 else 0
    // in `A*x = b`, A is `reqToBtns`, b is `req`, x is not known yet
    val btnIndices = reqToBtns.head.indices
    // perform Gaussian elimination - mutates req and reqToBtns to more solvable one
    btnIndices.foreach: btnIdx =>
      reqToBtns.indices.find(i =>
        reqToBtns(i)(btnIdx) != 0 &&
        reqToBtns(i).take(btnIdx).forall(_ == 0)
      ).foreach: pivotIdx =>
        val pivot = reqToBtns(pivotIdx)(btnIdx)
        reqToBtns.indices.filter(_ != pivotIdx).foreach: curIdx =>
          // using doubles works, but comparisons become messy, so we avoid using them
          // to ensure that the current cell is divisible by the pivot, we upscale
          //   the current row by the minimal amount that makes it so - `mult1`.
          val mult1 = pivot / BigInt(pivot).gcd(reqToBtns(curIdx)(btnIdx)).toInt
          val mult2 = (reqToBtns(curIdx)(btnIdx) * mult1) / pivot
          reqToBtns(curIdx) = reqToBtns(curIdx).zip(reqToBtns(pivotIdx)).map(_ * mult1 - _ * mult2)
          req(curIdx) = req(curIdx) * mult1 - req(pivotIdx) * mult2
    // we will need to check all possible values of these
    // they are button presses for the corresponding buttons
    val freeVars = btnIndices.filter(i => reqToBtns.count(_(i) != 0) != 1).toArray
    // these are also button presses
    // however, they are directly calculated from the free vars
    // each element of this array is a 3-tuple of:
    // - remaining: Int - the right hand side of the equality
    // - thisBtns: Array[Int} - thisBtns * freeVars is on the left hand side
    // - the pivot cell's value, which is multiplied by this var itself, left hand side
    // after assigning the free vars, we derive the current var by
    //   whatever is leftover to multiply the pivot cell by
    // sometimes this is non-integer or negative, which means that the free vars are invalid
    val derivedVars = reqToBtns.indices.toArray.flatMap: i =>
      reqToBtns(i).find(_ != 0).map: pivotCell =>
        (req(i), reqToBtns(i), pivotCell)
    val possibleValues = 0.to(maxPresses.toInt).toVector
    val allCombinations = Vector.fill(freeVars.size)(possibleValues).sequence.view
    // for each combination of free vars, calculate the derived var and check
    //   the total presses to find the minimum, filtering out invalid solutions
    // this is the performance bottleneck
    // the optimized mutable code makes the runtime 4 times faster
    //   compared to the minimalist immutable version
    allCombinations.map(presses => {
      var sum = presses.sum
      var valid = true
      var i = 0
      while (valid && i < derivedVars.size)
        val (thisReq, thisBtns, pivotCell) = derivedVars(i)
        var remaining = thisReq
        var pressIdx = 0
        while (pressIdx < presses.size)
          remaining -= presses(pressIdx) * thisBtns(freeVars(pressIdx))
          pressIdx += 1
        sum += remaining / pivotCell
        // halt early if a positive integer solution is impossible
        if remaining % pivotCell != 0 || remaining / pivotCell < 0 then
          valid = false
        i += 1
      if valid then
        sum
      else
        Int.MaxValue
    }).min
  }).sum
  println(res)
