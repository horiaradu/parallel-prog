package week2

import common._

import scala.reflect.ClassTag

/**
  * Created by horiaradu on 25/09/2016.
  */
object parallelCollectionFunctionsOnTrees {

  sealed abstract class Tree[A] {
    val size: Int
  }

  case class Leaf[A](a: Array[A]) extends Tree[A] {
    override val size = a.length
  }

  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
    override val size = l.size + r.size
  }

  def mapTreePar[A, B: ClassTag](tree: Tree[A], f: A => B): Tree[B] =
    tree match {
      case Leaf(data) => {
        val result = new Array[B](data.length)
        for (i <- data.indices) result(i) = f(data(i))
        Leaf(result)
      }
      case Node(l, r) => {
        val leftTask = task {
          mapTreePar(l, f)
        }
        val rightTask = task {
          mapTreePar(r, f)
        }

        Node(leftTask.join, rightTask.join)
      }
    }

  def main(args: Array[String]): Unit = {
    val tree = Node(
      Node(
        Node(
          Leaf(Array(1, 2, 3)),
          Leaf(Array(4, 5, 6))
        ),
        Leaf(Array(7, 8, 9))
      ),
      Node(
        Leaf(Array(1, 2, 3)),
        Leaf(Array(1, 2, 3))
      )
    )
    mapTreePar(tree, (x: Int) => x + 1)
  }
}
