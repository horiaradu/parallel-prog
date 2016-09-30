package week2

import common._

/**
  * Created by horiaradu on 28/09/2016.
  */
object scan {
  def scanLeftSeq[A](input: Array[A], initialValue: A, f: (A, A) => A, out: Array[A]): Unit = {
    out(0) = initialValue
    var value = initialValue

    for {
      i <- 1 to input.length
    } {
      value = f(value, input(i - 1))
      out(i) = value
    }
  }

  def reduceSeg1[A](input: Array[A], left: Int, right: Int, initialValue: A, f: (A, A) => A): A = ???

  def mapSeg[A, B](input: Array[A], left: Int, right: Int, fi: (Int, A) => B, output: Array[B]): Unit = ???

  def scanLeft[A](input: Array[A], initialValue: A, f: (A, A) => A, output: Array[A]) = {
    val fi = { (i: Int, v: A) => reduceSeg1(input, 0, i, initialValue, f) }
    mapSeg(input, 0, input.length, fi, output)
    val last = input.length - 1
    output(last + 1) = f(output(last), input(last))
  }

  /** ************ with a tree ******************/

  sealed abstract class Tree[A]

  case class Leaf[A](a: A) extends Tree[A]

  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  sealed abstract class TreeRes[A] {
    val res: A
  }

  case class LeafRes[A](override val res: A) extends TreeRes[A]

  case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

  def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] =
    t match {
      case Leaf(a) => LeafRes(a)
      case Node(l, r) =>
        val (tL, tR) = (reduceRes(l, f), reduceRes(r, f))
        NodeRes(tL, f(tL.res, tR.res), tR)
    }

  //bottom up computation
  def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] =
  t match {
    case Leaf(a) => LeafRes(a)
    case Node(l, r) =>
      val (tL, tR) = parallel(upsweep(l, f), upsweep(r, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
  }

  def downsweep[A](t: TreeRes[A], reducedLeftElements: A, f: (A, A) => A): Tree[A] =
    t match {
      case LeafRes(a) => Leaf(f(reducedLeftElements, a))
      case NodeRes(l, _, r) =>
        val (tL, tR) = parallel(downsweep(l, reducedLeftElements, f),
          downsweep(r, f(reducedLeftElements, l.res), f))
        Node(tL, tR)
    }

  def prepend[A](x: A, t: Tree[A]): Tree[A] =
    t match {
      case Leaf(v) => Node(Leaf(x), Leaf(v))
      case Node(l, r) => Node(prepend(x, l), r)
    }

  def scanLeft[A](t: Tree[A], initialValue: A, f: (A, A) => A): Tree[A] = {
    val tRes = upsweep(t, f)
    val scan1 = downsweep(tRes, initialValue, f)
    prepend(initialValue, scan1)
  }

  def main(args: Array[String]): Unit = {
    println(List(1, 3, 8).scanLeft(0)(_ + _))

    println(List(1, 3, 8).scanRight(0)(_ + _))

    val out = new Array[Int](4)
    scanLeftSeq(Array(1, 3, 8), 0, (x: Int, y: Int) => x + y, out)
    println(out.foldLeft("")(_ + ", " + _))
  }
}
