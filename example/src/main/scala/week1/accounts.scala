package week1

import java.util.UUID

/**
  * Created by horiaradu on 24/09/2016.
  */
object accounts {
  def main(args: Array[String]): Unit = {
    class Account(private var amount: Int = 0) {
      val uid = UUID.randomUUID()

      def transfer(target: Account, amount: Int) =
        if (this.uid.compareTo(target.uid) < 0) this.lockAndTransfer(target, amount)
        else target.lockAndTransfer(this, -amount)

      private def lockAndTransfer(target: Account, amount: Int) = {
        this.synchronized {
          target.synchronized {
            println(s"transfering $amount from $this to $target")
            this.amount -= amount
            target.amount += amount
          }
        }
      }

      override def toString: String = uid.toString
    }

    def startThread(a: Account, b: Account, amount: Int) = {
      val t = new Thread {
        override def run(): Unit = {
          for (i <- 0 until amount) {
            a.transfer(b, 1)
          }
        }
      }

      t.start()
      t
    }

    val a1 = new Account(50000000)
    val a2 = new Account(90000000)
    val t = startThread(a1, a2, 20)
    val s = startThread(a2, a1, 20)
    t.join()
    s.join()
  }
}
