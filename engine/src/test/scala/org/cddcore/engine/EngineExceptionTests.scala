package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineExceptionTests extends EngineStringStringTests {

  "An engine" should "throw a MultipleExceptionIf needed, recording all the exceptions in the scenarioExceptionMap" in {
    val bldr = builderWithUseCase.
      scenario("one").because((s: String) => false, "always false").
      scenario("two").because((s: String) => false, "always false").
      scenario("three").expected("x").code((s: String) => "y").
      scenario("four").expected("x").code((s: String) => "y").
      scenario("ok").expected("x") //because((s: String) => s == "ok").
    val e = evaluating { bldr.build } should produce[MultipleExceptions]
    val scenarioExceptionMap = e.scenarioExceptionMap
    //TODO Copying up the description of the scenario in here is a bad thing TM
    val scenarios = bldr.useCases.reverse.flatMap((u) => u.scenarios.reverse)
    val sOne = scenarios(0)
    val stwo = scenarios(1)
    val sThree = scenarios(2)
    val sFour = scenarios(3)
    val sOk = scenarios(4)

    val eOne = scenarioExceptionMap(sOne)
    val eTwo = scenarioExceptionMap(stwo)
    val eThree = scenarioExceptionMap(sThree)
    val eFour = scenarioExceptionMap(sFour)
    assert(!scenarioExceptionMap.contains(sOk), scenarioExceptionMap)

    //    checkBecauseException(eOne);
    //    checkBecauseException(eTwo);

  }
  "An engine" should "record all the exceptions in the scenarioExceptionMap, if testing" in {
    val engine = EngineTest.test(() =>
      builderWithUseCase.
        scenario("one").because((s: String) => false, "always false").
        scenario("two").because((s: String) => false, "always false").
        scenario("three").expected("x").code((s: String) => "y").
        scenario("four").expected("x").code((s: String) => "y").
        scenario("ok").expected("x"). //because((s: String) => s == "ok").
        build)
    val sOne = engine.scenarios(0)
    val stwo = engine.scenarios(1)
    val sThree = engine.scenarios(2)
    val sFour = engine.scenarios(3)
    val sOk = engine.scenarios(4)

    val eOne = engine.scenarioExceptionMap(sOne)
    val eTwo = engine.scenarioExceptionMap(stwo)
    val eThree = engine.scenarioExceptionMap(sThree)
    val eFour = engine.scenarioExceptionMap(sFour)
    assert(!engine.scenarioExceptionMap.contains(sOk), engine.scenarioExceptionMap)

    //    checkBecauseException(eOne);
    //    checkBecauseException(eTwo);

  }
  
 
} 