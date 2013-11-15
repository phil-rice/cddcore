package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioTests extends EngineStringStringTests {

  "An empty engine" should "allow the first use not to have a because, and become the default value" in {
    println(engineWithDefault)
    assertEquals(1, engineWithDefault.scenarios.size)
    assertEngineMatches(engineWithDefault, Left(CodeAndScenarios("Z": Code, List(defaultScenario))));
    assertEquals("Z", engineWithDefault("A"))
    assertEquals("Z", engineWithDefault("B"))
  }

  it should " allow the first use not to have a because, and become the default value when we add a second scenario in a second use case" in {
    val e = Engine[Int, String]().
      useCase("").scenario(1).expected("x").
      useCase("").scenario(2).expected("y").because((x: Int) => x == 2).
      build
  }
  it should " allow the first use not to have a because, and become the default value when we add a second scenario in same used case" in {
    val e = Engine[Int, String]().
      useCase("").scenario(1).expected("x").
      scenario(2).expected("y").because((x: Int) => x == 2).
      build
  }

  it should "produce a simple if then with two scenarios" in {
    val b = builderWithDefault.scenario("B").because("B").expected("X");
    val e = b.build
    assertEquals(2, e.scenarios.size)
    assertEquals(defaultScenario, e.scenarios(0))
    val bScenario = e.scenarios(1)
    assertEngineMatches(e, Right(EngineNode(because = List("B"), inputs = List("B"), yes = Left(CodeAndScenarios("X", List(bScenario))), no = Left(CodeAndScenarios("Z", List(defaultScenario))), scenarioThatCausedNode = bScenario)))
  }

  it should "Throw ScenarioConflictingWithDefaultException if second scenario is assertion and comes to wrong result" in {
    val b = builderWithDefault.scenario("A").expected("X");
    evaluating {
      b.build
    } should produce[ScenarioConflictingWithDefaultException]
  }

  it should "Add scenario to root if adding assertion" in {
    val b = builderWithDefault.scenario("B").expected("Z")
    val e1 = builderWithDefault.build
    val e2 = b.build
    val bScenario = e2.scenarios(1)
    assertEngineMatches(e1, Left(CodeAndScenarios("Z", List(defaultScenario))))
    assertEngineMatches(e2, Left(CodeAndScenarios("Z", List(bScenario, defaultScenario))))
  }

  it should "Add scenario to root if adding with same conclusion, different reason" in {
    val b = builderWithDefault.scenario("B").because("B").expected("Z")
    val e1 = builderWithDefault.build
    val e2 = b.build
    val bScenario = e2.scenarios(1)
    assertEngineMatches(e1, Left(CodeAndScenarios("Z", List(defaultScenario))))
    assertEngineMatches(e2, Left(CodeAndScenarios("Z", List(bScenario, defaultScenario))))
  }

  it should "Throw NoExpectedException if scenario doesnt have expected" in {
    evaluating { builderWithScenario.code("Z").build } should produce[NoExpectedException]
    //    evaluating { builderWithDefault.because("A").code("X").build } should produce[NoExpectedException]
  }

  it should "Throw ScenarioBecauseException if because is not true in scenario" in {
    val b = builderWithDefault.scenario("B").because("X")
    //    b.build
    evaluating { b.build } should produce[ScenarioBecauseException]
  }

}