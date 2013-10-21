package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineDescriptionTests extends AbstractTest {

  "A builder" should "keep the engineDescription" in {
    val engine1 = Engine[Int, Int]().withDescription("someDescription").useCase("uc").scenario(1).expected(1).build
    val engine2 = Engine[Int, Int, Int]().withDescription("someDescription").useCase("uc").scenario(1, 1).expected(1).build
    val engine3 = Engine[Int, Int, Int, Int]().withDescription("someDescription").useCase("uc").scenario(1, 1, 1).expected(1).build
    assertEquals(engine1.engineDescription, Some("someDescription"))
    assertEquals(engine2.engineDescription, Some("someDescription"))
    assertEquals(engine3.engineDescription, Some("someDescription"))
  }

}