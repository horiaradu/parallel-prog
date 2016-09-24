package week1

import org.scalameter._

/**
  * Created by horiaradu on 24/09/2016.
  */
object benchmark {
  def main(args: Array[String]): Unit = {
    val time = measure {
      (0 until 100000000).toArray
    }

    println(s"Array inti time $time ms")
  }
}
