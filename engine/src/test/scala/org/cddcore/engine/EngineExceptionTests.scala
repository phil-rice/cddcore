package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
@RunWith(classOf[JUnitRunner])
class EngineExceptionTests extends EngineStringStringTests {
  implicit def eToEngineFromTests[R](e: Engine) = e.asInstanceOf[EngineFromTestsImpl]

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
    val scenarios = bldr.builderData.all(classOf[UseCase]).reverse.flatMap((u) => u.children.reverse.asInstanceOf[List[Scenario]])
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
    val engine = org.cddcore.engine.Engine.test {
      builderWithUseCase.
        scenario("one").because((s: String) => false, "always false").
        scenario("two").because((s: String) => false, "always false").
        scenario("three").expected("x").code((s: String) => "y").
        scenario("four").expected("x").code((s: String) => "y").
        scenario("ok").expected("x"). //because((s: String) => s == "ok").
        build
    }
    val sOne = engine.tests(0)
    val stwo = engine.tests(1)
    val sThree = engine.tests(2)
    val sFour = engine.tests(3)
    val sOk = engine.tests(4)

    val eOne = engine.scenarioExceptionMap(sOne)
    val eTwo = engine.scenarioExceptionMap(stwo)
    val eThree = engine.scenarioExceptionMap(sThree)
    val eFour = engine.scenarioExceptionMap(sFour)
    assert(!engine.scenarioExceptionMap.contains(sOk), engine.scenarioExceptionMap)

    //    checkBecauseException(eOne);
    //    checkBecauseException(eTwo);

  }
  it should "Remember DuplicateScenarioException when executing in test mode" in {
    val b = org.cddcore.engine.Engine[Int, String]().
      useCase("").scenario(1).expected("x").
      scenario(1).expected("x")
    val e = Engine.test { b.build }
    val sOne = e.tests(0)
    val eOne = e.scenarioExceptionMap(sOne).asInstanceOf[DuplicateScenarioException]
    assertEquals(sOne, eOne.scenario)
  }
} 