package org.cddcore.utilities

import org.junit.runner.RunWith
import java.util.concurrent.CountDownLatch
import org.cddcore.engine.AbstractTest
import org.cddcore.engine.Engine
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ProfilerTest extends AbstractTest {

  "A Simpleprofiler" should "add up the elapsed time, and count of times called in a single thread " in {
    val p = new SimpleProfiler[String]()
    p.startWithTime(10, "a")
    p.endWithTime(110, "a")

    p.startWithTime(20, "b")
    p.endWithTime(220, "b")
    val actual = p.results
    assertEquals(Map() + ("a" -> ProfilerRecord(1, 100), "b" -> ProfilerRecord(1, 200)), actual)
  }

  it should "ignore nested times" in {
    val p = new SimpleProfiler[String]()
    p.startWithTime(10, "a")
    p.endWithTime(110, "a")

    p.startWithTime(20, "b")
    p.startWithTime(25, "b")
    p.endWithTime(50, "b")
    p.startWithTime(65, "b")
    p.endWithTime(80, "b")
    p.endWithTime(220, "b")
    val actual = p.results
    assertEquals(Map() + ("a" -> ProfilerRecord(1, 100), "b" -> ProfilerRecord(3, 200)), actual)
  }

  class ProfileTestThread(p: Profiler[String], latch: CountDownLatch) extends Thread {
    override def run {
      latch.await()
      p.startWithTime(10, "a")
      p.endWithTime(110, "a")

      p.startWithTime(20, "b")
      p.endWithTime(220, "b")
    }
  }

  "A multithreaded Profiler" should "add up the elapsed time, and count of times called in a single thread " in {
    val p = new ThreadedProfiler[String]()
    p.startWithTime(10, "a")
    p.endWithTime(110, "a")

    p.startWithTime(20, "b")
    p.endWithTime(220, "b")
    val actual = p.results
    assertEquals(Map() + ("a" -> ProfilerRecord(1, 100), "b" -> ProfilerRecord(1, 200)), actual)
  }

  it should "ignore nested times" in {
    val p = new ThreadedProfiler[String]()
    p.startWithTime(10, "a")
    p.endWithTime(110, "a")

    p.startWithTime(20, "b")
    p.startWithTime(25, "b")
    p.endWithTime(50, "b")
    p.startWithTime(65, "b")
    p.endWithTime(80, "b")
    p.endWithTime(220, "b")
    val actual = p.results
    assertEquals(Map() + ("a" -> ProfilerRecord(1, 100), "b" -> ProfilerRecord(3, 200)), actual)
  }

  it should "fold times across threads" in {
    val p = new ThreadedProfiler[String]
    val l = new CountDownLatch(1)
    val threads = 1 to 10 map ((i) => new ProfileTestThread(p, l))
    threads.foreach(_.start)
    l.countDown()
    threads.foreach(_.join)
    assertEquals(Map() + ("a" -> ProfilerRecord(10, 1000), "b" -> ProfilerRecord(10, 2000)), p.results)
  }

  "An engine called with a profiler" should "return Either(result or exception) and the profiler" in {
    val e = Engine[Int, String].code((x: Int) => "HelloWorld").build
    val (r, res) = Engine.profile {
      val result = e(1)
      result
    }
    assertEquals(Right("HelloWorld"), r);
    val pr = res.results
    assertEquals(1, pr.size)
    assertEquals(1, pr(e).count)
  }

  it should "return the result and profiler" in {
    val e = Engine[Int, String].code((x: Int) => "HelloWorld").build
    val (r, res) = Engine.profileNoException { e(1) }
    assertEquals("HelloWorld", r);
    assertEquals(1, res.results.size)
    assertEquals(1, res.results(e).count)

  }
}