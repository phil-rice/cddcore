package org.cddcore.engine

import org.cddcore.engine._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BuilderPriorityTests extends AbstractTest {
  implicit def toEngineFromTests[R](e: Engine) = e.asInstanceOf[EngineBuiltFromTests[R]]
  "A builder" should "allow priority to be set in scenario, usecase or engine, and that be reflected in the scenarios produced" in {
    val b = Engine[String, Int]().priority(-3).
      useCase("uc1").priority(1).
      scenario("uc1_1").expected(1).
      scenario("uc1_2").expected(1).priority(5).
      useCase("uc2").
      scenario("uc2_1").expected(1)
    val c = b.builderData.childrenModifiedForBuild
    val e = b.build
    def s(name: String) = e.tests.find(_.params == List(name)).get
    assertEquals(Some(1), s("uc1_1").priority)
    assertEquals(Some(5), s("uc1_2").priority)
    assertEquals(Some(-3), s("uc2_1").priority)
  }
}