import java.util.concurrent.ForkJoinTask

import common._

object session {
  def sum(xs: Array[Int]): Int =
    xs.par.foldLeft(0)(_ + _)

  def max(xs: Array[Int]): Int =
    xs.par.fold(Int.MinValue)(_ max _)

  /**
    * Preconditions of fold:
    *
    * f(a, f(b, c)) == f(f(a, b), c)
    * f(z, a) = f(a, z) = a
    *
    * => Monoid
    **/

  trait Iterator[T] {
    def hasNext: Boolean

    def next(): T

    def foldLeft[S](z: S)(f: (S, T) => S): S = {
      def foldLeftRec(acc: S): S = {
        if (hasNext) foldLeftRec(f(acc, next()))
        else acc
      }

      foldLeftRec(z)
    }
  }

  trait Splitter[T] extends Iterator[T] {
    def split: Seq[Splitter[T]]

    def remaining: Int

    /** ****/
    val threshold = 4

    def fold(z: T)(f: (T, T) => T): T = {
      if (remaining < threshold) foldLeft(z)(f)
      else {
        val children: Seq[ForkJoinTask[T]] = split
          .map(part => common.task {
            part.fold(z)(f)
          })
        children.map(_.join()).foldLeft(z)(f)
      }
    }
  }

  /**
    * build collections
    *
    * @tparam A    type of elems
    * @tparam Repr type of collection
    */
  trait Builder[A, Repr] {
    def +=(elem: A): Builder[A, Repr]

    def result: Repr
  }

  /**
    * each collection has method:
    * def newBuilder: Builder[A, Repr]
    *
    */

  trait Traversable[T] {
    def foreach(f: T => Unit): Unit

    def newBuilder: Builder[T, Traversable[T]]

    def filter(p: T => Boolean): Traversable[T] = {
      val builder = newBuilder
      for (elem <- this) if (p(elem)) builder += elem
      builder.result
    }
  }

  /** * parallel ***/
  trait Combiner[A, Repr] extends Builder[A, Repr] {
    def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
  }

  /**
    * each PARALLEL collection has method:
    * def newCombiner: Combiner[A, Repr]
    *
    */

}