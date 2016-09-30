package week2

import common._

/**
  * Created by horiaradu on 25/09/2016.
  */
object mergeSort {
  def main(args: Array[String]): Unit = {
    val xs = List(9, 8, 7, 6, 5, 4, 3, 2, 1)
    val ys = List.empty
    val maxDepth = 3

    def quickSort(xs: List[Int]): List[Int] = xs.sorted

    def sort(xs: List[Int]): List[Int] = {
      def sortWithDepth(xs: List[Int], depth: Int): List[Int] = {
        if (depth == maxDepth) quickSort(xs)
        else {
          val mid = xs.length / 2
          val (fst, snd) = xs.splitAt(mid)

          val fstTask = task {
            sortWithDepth(fst, depth + 1)
          }
          val sndTask = task {
            sortWithDepth(snd, depth + 1)
          }

          merge(fstTask.join, sndTask.join)
        }
      }

      sortWithDepth(xs, 0)
    }

    def merge(first: List[Int], second: List[Int]): List[Int] = {
      def mergeRec(acc: List[Int], first: List[Int], second: List[Int]): List[Int] =
        (first, second) match {
          case (Nil, _) => acc ++ second
          case (_, Nil) => acc ++ first
          case (fstHead :: fstTail, sndHead :: sndTail) =>
            if (fstHead <= sndHead) mergeRec(acc :+ fstHead, fstTail, second)
            else mergeRec(acc :+ sndHead, first, sndTail)
        }

      mergeRec(Nil, first, second)
    }

    println(sort(xs))
  }
}
