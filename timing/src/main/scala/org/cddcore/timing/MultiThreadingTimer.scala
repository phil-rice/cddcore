package org.cddcore.timing

import org.cddcore.carers._
import scala.concurrent.stm._
trait PerformanceTimer {

  def nanos: Long
  def getDataItem(i: Int): List[Any]
  def call(params: List[Any])

  def time(numberOfThreads: Int, warmUp: Int, count: Int) = {
    var threads = List[Thread]()
    var result = Ref[Map[Thread, Long]](Map())
    val countsPerThread = count / numberOfThreads
    for (threadId <- 1 to numberOfThreads) {
      val t = new Thread() {
        val params = getDataItem(0)
        override def run: Unit = {
          time(warmUp)
          val duration = time(countsPerThread)
          atomic { implicit inTxn => result.transform((f) => f + (Thread.currentThread -> duration)) }
        }

        private def time(n: Int) = {
          var sum: Long = 0
          for (i <- 0 to n) {
            val start = nanos
            call(params)
            val duration = nanos - start
            sum += duration
          }
          sum
        }
      }
      threads = t :: threads
    }
    threads.foreach(_.start)
    threads.foreach(_.join)
    atomic { implicit inTxn => result.get.mapValues(_ / 1000.0 / countsPerThread) }
  }
}

trait SystemPerformanceTimer {
  def nanos = System.nanoTime()
}

class CarersPerformanceTimer extends PerformanceTimer with SystemPerformanceTimer {
  final val engine = Carers.engine

  def getDataItem(i: Int): List[Any] = List(CarersXmlSituation(World("2010-6-9"), Xmls.validateClaim("CL100104A")))
  def call(params: List[Any]) {
    engine(params(0).asInstanceOf[CarersXmlSituation])
  }
}

object CarersTimer {
  def main(args: Array[String]) {
    val carers = new CarersPerformanceTimer
    println("Warming")
    carers.time(4, 100000, 0)
    println("Starting")
    for (numberOfThreads <- 1 to 8) {
      val warmUp = 1
      val count = 100000
      val result = carers.time(numberOfThreads, warmUp, count)
      println(f"Threads $numberOfThreads, warmUp $warmUp, count $count,\n  ${result.mkString("\n  ")} us/situation")
    }
  }

}