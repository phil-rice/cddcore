package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExceptionMessageTests extends EngineStringStringTests {

  "An engine" should "Throw ScenarioConflictingWithDefaultException if second scenario is assertion and comes to wrong result" in {
    val b = builderWithDefault.scenario("A").expected("X");
    val e = evaluating { b.build } should produce[ScenarioConflictingWithDefaultException]
    assertEquals("A", e.scenario.params(0))
    //    assertEquals(b.scenariosForBuild(0), e.scenario)
    assertEquals(b.builderData.scenariosForBuild(1), e.scenario)
    assertEquals("\nActual Result: Z\n" +
      "Expected X\n" +
      " Scenario Scenario(, A, because=, expected=X)\n" +
      "Detailed:\n" +
      "  List(A)", e.getMessage())
  }

  it should "Throw NoExpectedException if scenario doesnt have expected" in {
    val b = builderWithScenario.code("Z")
    val e = evaluating { b.build } should produce[NoExpectedException]
    assertEquals(b.builderData.scenariosForBuild(0), e.scenario)
    assertEquals("No expected in Scenario(, W, because=, expected=<N/A>)\n" +
      "Detailed:\n" +
      "  List(W)", e.getMessage())
  }

  it should "Throw ScenarioBecauseException if because is not true in scenario" in {
    val b = builderWithDefault.scenario("B").because("X")
    val e = evaluating { b.build } should produce[ScenarioBecauseException]
    assertEquals(b.builderData.scenariosForBuild(1), e.scenario)
    assertEquals("X is not true for Scenario(, B, because=X, expected=<N/A>)\n" +
      "Detailed:\n" +
      "  List(B)\n", e.getMessage())
  }


  it should "throw ScenarioConflictException if it cannot decide between two scenarios" in {
    val bldr = builderWithDefault.
      scenario("AB").because("A").expected("X").
      scenario("AB").because("B").expected("Y");
    val e = evaluating { bldr.build } should produce[ScenarioConflictException]
    assertEquals("Cannot differentiate based on:\n" +
      " B\n" +
      "Existing: AB\n" +
      "Being Added: AB\n" +
      "Detailed existing:\n" +
      "Scenario(, AB, because=A, expected=X)\n" +
      "Detailed of being Added:\n" +
      "Scenario(, AB, because=B, expected=Y)", e.getMessage())
  }

  it should "Throw ScenarioConflictingWithDefaultException if engine just has default output and scenario comes to wrong conclusion" in {
    val bldr = builderWithDefault.
      scenario("AB").expected("X")

    val e = evaluating { bldr.build } should produce[ScenarioConflictingWithDefaultException]
    assertEquals(e.scenario, bldr.builderData.scenariosForBuild(1))
    assertEquals("\nActual Result: Z\n" +
      "Expected X\n" +
      " Scenario Scenario(, AB, because=, expected=X)\n" +
      "Detailed:\n" +
      "  List(AB)", e.getMessage())
  }

}