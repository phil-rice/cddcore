package org.cddcore.tests

import org.cddcore.engine.Engine
import org.junit.runner.RunWith

@RunWith(classOf[CddJunitRunner])
object SampleBrokenEngine {
  val e = Engine[Int, Int]().title("broken").scenario(0, "fails").expected(1).code(_ + 1).build
}
