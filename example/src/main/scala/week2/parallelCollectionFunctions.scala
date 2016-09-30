package week2

import common._

/**
  * Created by horiaradu on 25/09/2016.
  */
object parallelCollectionFunctions {
  def mapASegmentSeq[A, B](input: Array[A], left: Int, right: Int, f: A => B, output: Array[B]): Unit =
    for (i <- left until right) output(i) = f(input(i))

  val threshold = 2

  def mapASegmentPar[A, B](input: Array[A], left: Int, right: Int, f: A => B, output: Array[B]): Unit = {
    if (right - left < threshold) mapASegmentSeq(input, left, right, f, output)
    else {
      val mid = left + (right - left) / 2
      parallel(
        mapASegmentPar(input, left, mid, f, output),
        mapASegmentPar(input, mid, right, f, output)
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val out = new Array[Int](4)
    mapASegmentSeq(Array(1, 2, 3, 4), 0, 4, (x: Int) => x + 1, out)
    out.foreach(o => print(o))
  }
}
