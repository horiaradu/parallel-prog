package week4

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import common._

import scala.collection.parallel.Combiner

/**
  * Created by horiaradu on 10/10/2016.
  */
class ArrayCombiner[T <: AnyRef : ClassTag](val parallelism: Int) extends Combiner[T, Array[T]] {
  private var numElems = 0
  private val buffers = new ArrayBuffer[ArrayBuffer[T]]()
  buffers += new ArrayBuffer[T]()

  def +=(x: T) = {
    buffers.last += x
    numElems += 1
    this
  }

  def combine(that: ArrayCombiner[T]) = {
    buffers ++= that.buffers
    numElems += that.numElems
    this
  }

  def result: Array[T] = {
    val array = new Array[T](numElems)
    val step = math.max(1, numElems / parallelism)

    val starts = (0 until numElems by step) :+ numElems
    val chunks = starts.zip(starts.tail)

    val tasks = for ((from, end) <- chunks) yield task {
      copyTo(array, from, end)
    }
    tasks.foreach(_.join())
    array
  }

  private def copyTo(array: Array[T], from: Int, end: Int): Unit = {
    var i = from
    var j = 0
    while (i >= buffers(j).length) {
      i -= buffers(j).length
      j += 1
    }
    var k = from
    while (k < end) {
      array(k) = buffers(j)(i)
      i += 1
      if (i >= buffers(j).length) {
        i = 0
        j += 1
      }
      k += 1
    }
  }

  override def combine[N <: T, NewTo >: Array[T]](other: Combiner[N, NewTo]): Combiner[N, NewTo] = ???

  override def clear(): Unit = ???

  override def size: Int = ???
}

object Combiners {

}
