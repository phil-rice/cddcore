package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineBecauseTests  extends AbstractTest{
  
  "An exception in a because clause" should "add throw a BecauseClauseException" in {
    import EngineWithScenarioExceptionMap._
    val e = new RuntimeException
    val b = Engine[Int,String].scenario(0).expected("x").because((x: Int) => throw e)
    evaluating { b.build } should produce[BecauseClauseException]
    val engine = Engine.test{b.build}
    val s = engine.scenarioExceptionMap
  }

}