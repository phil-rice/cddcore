package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.concurrent.atomic._
import java.util.concurrent._

@RunWith(classOf[JUnitRunner])
class MapsTests extends AbstractTest {

  "the walkSelfAndChildrenPaths" should "return an  iterable of paths including the root" in {
    val map = Map[String, List[String]]("a" -> List("b", "c"), "b" -> List("d", "e"))
    assertEquals(List(List("a"), List("a", "b"), List("a", "b", "d"), List("a", "b", "e"), List("a", "c")), Maps.walkSelfAndChildrenPaths(map)("a"))
  }

  class MapsCacheTestThread(val cache: AtomicReference[Map[String, Future[Int]]], count: AtomicInteger, val waitLatch: CountDownLatch) extends Thread {
    var result = 0
    override def run {
      result = Maps.getOrCreate[String, Int](cache, "key", {
        waitLatch.await(10, TimeUnit.SECONDS)
        count.incrementAndGet()
      })
    }
  }

  "The threadsafe cache" should "only call the value closure once" in {
    val maxThreads = 10
    var threads = List[MapsCacheTestThread]()
    val cache = new AtomicReference[Map[String, Future[Int]]](Map())
    val count = new AtomicInteger(0)
    val wait = new CountDownLatch(1)
    for (i <- 1 to maxThreads)
      threads = {
        val t = new MapsCacheTestThread(cache, count, wait)
        t.start
        t :: threads
      }
    wait.countDown()
    for (t <- threads) {
      t.join
      assertEquals(1, t.result)
    }
  }

  it should "work if spammed from the same thread" in {
    val cache = new AtomicReference[Map[String, Future[Int]]](Map())
    val count = new AtomicInteger(0)
    for (i <- 1 to 10)
      assertEquals(1, Maps.getOrCreate(cache, "1", count.incrementAndGet()))
  }
}