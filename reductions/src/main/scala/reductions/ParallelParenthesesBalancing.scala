package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def charToIncrement(char: Char) =
      if (char == '(') 1
      else if (char == ')') -1
      else 0

    @tailrec
    def balanceRec(chars: Array[Char], count: Int, from: Int): Boolean =
      if (count < 0) false
      else if (from < chars.length) {
        val nextCount = charToIncrement(chars(from)) + count
        balanceRec(chars, nextCount, from + 1)
      }
      else count == 0


    balanceRec(chars, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, depth: Int, gain: Int): (Int, Int) = {
      var currentGain = depth
      var currentDepth = gain
      for (i <- idx until until) {
        if (chars(i) == '(') {
          currentDepth += 1
          currentGain += 1
        } else if (chars(i) == ')') {
          if (currentGain > 0) currentDepth -= 1
          currentGain -= 1
        }
      }
      (currentDepth, currentGain)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val ((leftDepth, leftGain), (rightDepth, rightGain)) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )

        val minDepth = Math.min(leftDepth, leftGain + rightDepth)
        val totGain = leftGain + rightGain
        (leftDepth + rightGain, leftGain + rightGain)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
