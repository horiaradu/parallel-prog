package week1

import java.lang.Math._

import common._

/**
  * Created by horiaradu on 24/09/2016.
  */
object segments {
  def main(args: Array[String]): Unit = {
    def sumSegment(a: Array[Int], p: Double, start: Int, end: Int): Int = {
      val powers = for (i <- start until end) yield power(a(i), p)
      powers.sum
    }

    def power(x: Int, p: Double) = math.exp(p * math.log(abs(x))).toInt

    def pNorm(a: Array[Int], p: Double): Int =
      power(sumSegment(a, p, 0, a.length), 1 / p)

    def pNormTwoPart(a: Array[Int], p: Double): Int = {
      val m = a.length / 2
      val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
      power(sum1 + sum2, 1 / p)
    }

    val threshold = 4

    def segmentRec(a: Array[Int], p: Double, start: Int, end: Int): Int =
      if (end - start < threshold) {
        sumSegment(a, p, start, end)
      } else {
        val m = start + (end - start) / 2
        val (sum1, sum2) = parallel(segmentRec(a, p, start, m), segmentRec(a, p, m, end))
        sum1 + sum2
      }

    def pNormRec(a: Array[Int], p: Double): Int =
      power(segmentRec(a, p, 0, a.length), 1 / p)

    val array = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(pNorm(array, 2))
    println(pNormTwoPart(array, 2))
    println(pNormRec(array, 2))
  }
}
