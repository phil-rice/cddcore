package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine._

@RunWith(classOf[JUnitRunner])
class EngineAssertionTests extends EngineStringStringTests {
import org.cddcore.engine.Engine._
  "Assertions" should "be executed once when testing" in {
    test { checkAssertionExecuted(1) }
  }

  "Assertions" should "be executed twice when not testing - one once building scenario, and once at end " in {
    checkAssertionExecuted(2)
  }

  def checkAssertionExecuted(times: Int) {
    var count = 0
    val a: A = (p: String, r:  ROrException[String]) => {
      count += 1;
      true
    }
    val assertion = new Assertion[A](assertion = a, description = "descr")

    val bldr = builderWithDefault.scenario("AB").because(becauseA).expected("X").assertion(assertion);

    assertEquals(0, count)
    val e = bldr.build
    assertEquals(times, count)
  }

  "Assertions" should "cause the build to fail with an AssertionException if they return false " in {
    val assertion = new Assertion[A](assertion = (p, r) => false, description = "descr")
    val bldr = builderWithDefault.scenario("AB").because(becauseA).expected("X").assertion(assertion);
    val e = evaluating { val e = bldr.build } should produce[AssertionException]

    assertEquals("\nAssertion descr failed.\nParams are List(AB)\nResult was X", e.getMessage())

  }

}