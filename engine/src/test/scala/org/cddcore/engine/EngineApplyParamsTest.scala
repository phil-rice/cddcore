package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class EngineApplyParamsTest extends AbstractTest {

  val e = Engine[Int, String]().
    scenario(0).expectException(new NullPointerException).code((x: Int) => throw new NullPointerException).because((x: Int) => x == 0).
    scenario(1).expected("One").because((x: Int) => x == 1).
    build.asInstanceOf[EngineWithResult[String]]

  "An engine" should "return the result of the engine if the applyParams method is called" in {
    assertEquals("One", e.applyParams(List(1)));
    evaluating { e.applyParams(List(0)) } should produce[NullPointerException]
    evaluating { e.applyParams(List(2)) } should produce[UndecidedException]
  }

  it should "wrap the results in an RorException if safeApplyParams is called" in {
    assertEquals(ROrException("One"), e.safeApplyParams((List(1))))
    assertEquals(ROrException(new NullPointerException), e.safeApplyParams(List(0)));
    assertEquals(ROrException(new UndecidedException), e.safeApplyParams(List(2)));
  }

}