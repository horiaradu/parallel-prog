package week2

import common._

/**
  * Created by horiaradu on 25/09/2016.
  */
object reduceTree {

  sealed abstract class Tree[A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def reduce[A](t: Tree[A], f: (A, A) => A): A =
    t match {
      case Leaf(v) => v
      case Node(left, right) => f(reduce(left, f), reduce(right, f))
    }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Node(left, right) => Node(map(left, f), map(right, f))
    }

  def toList[A](t: Tree[A]): List[A] = {
    val treeOfLists = map[A, List[A]](t, List(_))
    reduce[List[A]](treeOfLists, _ ++ _)
  }

  def reducePar[A](t: Tree[A], f: (A, A) => A): A =
    t match {
      case Leaf(v) => v
      case Node(left, right) =>
        val (reducedLeft, reducedRight) = parallel(reduce(left, f), reduce(right, f))
        f(reducedLeft, reducedRight)
    }

  /**
    * IF
    * f: (A, A) => A is associative,
    * t1: Tree[A],
    * t2: Tree[A],
    * toList(t1) == toList(t2)
    * THEN:
    * reduce(t1, f) == reduce(t2, f)
    */

  def main(args: Array[String]): Unit = {
    val tree = Node(
      Node(
        Leaf(3),
        Leaf(2)
      ),
      Node(
        Leaf(1),
        Leaf(4)
      )
    )

    val sum = reducePar(tree, (x: Int, y: Int) => x + y)
    println(sum)

    val list = toList(tree)
    println(list)


  }
}
