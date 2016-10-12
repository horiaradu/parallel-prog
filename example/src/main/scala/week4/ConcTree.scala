package week4

/**
  * Created by horiaradu on 12/10/2016.
  */
sealed trait Conc[+T] {
  def level: Int

  def size: Int

  def left: Conc[T]

  def right: Conc[T]

  def <>[S >: T](that: Conc[S]): Conc[S] = {
    if (this == Empty) that
    else if (that == Empty) this
    else Conc.concat(this, that)
  }
}

object Conc {
  def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff <= 1 && diff >= -1) new <>(xs, ys)
    else if (diff < -1) {
      if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else {
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nrr)
          new <>(nl, nr)
        } else {
          val nl = new <>(xs.left, xs.right.left)
          val nr = nrr
          new <>(nl, nr)
        }
      }
    } else {
      if (ys.right.level >= ys.left.level) {
        val nl = concat(xs, ys.left)
        new <>(nl, ys.right)
      } else {
        val nll = concat(xs, ys.left.left)
        if (nll.level == ys.level - 3) {
          val nl = new <>(nll, ys.left.right)
          val nr = ys.right
          new <>(nl, nr)
        } else {
          val nl = nll
          val nr = new <>(ys.left.right, ys.right)
          new <>(nl, nr)
        }
      }
    }
  }

  def appendLeaf[T](xs: Conc[T], y: T): Conc[T] = Append(xs, Single(y))
}

case object Empty extends Conc[Nothing] {
  def level = 0

  def size = 0

  def left = Empty

  def right = Empty
}

case class Single[T](x: T) extends Conc[T] {
  def level = 0

  def size = 1

  def left = Empty

  def right = Empty
}

/**
  * Invariants:
  * can never contain Empty
  *
  * @param left
  * @param right
  * @tparam T
  */
case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  def level = 1 + math.max(left.level, right.level)

  def size = 1 + left.size + right.size
}


/**
  * Invariants:
  * can never contain Empty
  * difference in level btw left and right is <= 1
  *
  * @param left
  * @param right
  * @tparam T
  */
case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  def level = 1 + math.max(left.level, right.level)

  def size = 1 + left.size + right.size
}

object ConcTree {
  def main(args: Array[String]): Unit = {

  }
}
