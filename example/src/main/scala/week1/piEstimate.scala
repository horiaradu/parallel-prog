package week1

import scala.util.Random
import common._
import org.scalameter._

/**
  * Created by horiaradu on 24/09/2016.
  */
object piEstimate {
  def mcCount(iter: Int): Int = {
    val randomX = new Random
    val randomY = new Random
    var hits = 0
    for (i <- 0 until iter) {
      val x = randomX.nextDouble
      val y = randomY.nextDouble
      if (x * x + y * y < 1) hits += 1
    }
    hits
  }

  def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

  def monteCarloPiPar(iter: Int): Double = {
    val ((pi1, pi2), (pi3, pi4)) =
      parallel(
        parallel(mcCount(iter / 4), mcCount(iter / 4)),
        parallel(mcCount(iter / 4), mcCount(iter - iter * 3 / 4))
      )

    4.0 * (pi1 + pi2 + pi3 + pi4) / iter
  }

  def monteCarloPiParWithTasks(iter: Int): Double = {
    val t1 = task {
      mcCount(iter / 4)
    }
    val t2 = task {
      mcCount(iter / 4)
    }
    val t3 = task {
      mcCount(iter / 4)
    }
    val t4 = task {
      mcCount(iter - iter * 3 / 4)
    }

    4.0 * (t1 + t2 + t3 + t4) / iter
  }

  def main(args: Array[String]): Unit = {
    val iterations: Int = 99999999
    //    println(s"seq: ${monteCarloPiSeq(iterations)}")
    //    println(s"par: ${monteCarloPiPar(iterations)}")
    //    println(s"par with tasks: ${monteCarloPiParWithTasks(iterations)}")

    //    val seqTime = measure {
    //      monteCarloPiSeq(iterations)
    //    }
    //    println(s"seq time: $seqTime")

    //    for (i <- 0 until 100) {
    //      val parTime = measure {
    //        monteCarloPiParWithTasks(iterations)
    //      }
    //      println(s"par time: $parTime")
    //    }

    //    val parTime = withWarmer(new Warmer.Default).measure {
    //      monteCarloPiParWithTasks(iterations)
    //    }
    //    println(s"par time: $parTime")

    val time = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 60,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)
      .measure {
        monteCarloPiParWithTasks(iterations)
      }
    println(s"par with warmup time: $time")
  }
}
