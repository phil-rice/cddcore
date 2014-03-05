package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger

class EngineCacheTests extends AbstractTest {

  "An engine with a cache" should "come to the same conclusions, but call the engine only once" in {
    val count = new AtomicInteger(0)
    val e = Engine[Int, String]().scenario(0).expected("0").code((x: Int) => { count.incrementAndGet; x.toString }).build.cached
    count.set(0)
    for (i <- 1 to 10) {
      assertEquals("0", e(0))
      assertEquals(i.toString, e(i))
    }
    assertEquals(11, count.get())
  }

}