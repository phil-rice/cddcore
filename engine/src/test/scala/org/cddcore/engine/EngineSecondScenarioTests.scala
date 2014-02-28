package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineSecondScenarioTests extends EngineStringStringTests {

  override val logger = new ConsoleLogger
  "An  engine" should "Add assertions to the no if scenario comes to correct value" in {
    val bldr = builderWithDefault.
      scenario("A").because("A").expected("X").
      scenario("B").expected("Z");
    val e = bldr.build

    val w = e.tests(0); assertEquals("", w.becauseString)
    val a = e.tests(1); assertEquals("A", a.becauseString)
    val b = e.tests(2); assertEquals("", b.becauseString)

    assertEngineMatches(e, Right(EngineNode(because = List(Left("A")), scenarioThatCausedNode = a, inputs = List("A"),
      yes = Left(CodeAndScenarios("X", List(a))),
      no = Left(CodeAndScenarios("Z", List(b, w))))))
  }
  it should "throw an ExpectedValueGotException exception if the default code is being used and second scenario has no because and doesn't throw an exception" in {
	  val bldr = builder.
			  scenario("A").because("A").expected("X").
			  scenario("B").expected("Z");
	  val e = evaluating { bldr.build } should produce[ExpectedValueGotException]
			  assertEquals("Z", e.expected);
	  assertEquals(classOf[UndecidedException], e.actual.getClass)
	  
  }

  it should "Add assertions to the yes if scenario comes to correct value" in {
    val b = builderWithDefault.
      scenario("A").because("A").expected("X").
      scenario("AB").expected("X")

    val e = b.build
    val u1 = b.builderData.children
    val u2 = b.builderData.childrenModifiedForBuild
    val s1 = e.tests
    val w = e.tests(0); assertEquals(List("W"), w.params)
    val a = e.tests(1); assertEquals(List("A"), a.params)
    val ab = e.tests(2); assertEquals(List("AB"), ab.params)

    assertEngineMatches(e, Right(EngineNode(because = List(Left("A")), inputs = List("A"), yes = Left(CodeAndScenarios("X", List(ab, a))), no = Left(CodeAndScenarios("Z", List(w))), scenarioThatCausedNode = a)))
  }

  it should "add to yes clause if because is true for root" in {
    val e = builderWithDefault.
      scenario("A").because("A").expected("X").
      scenario("AB").because("B").expected("Y").
      build

    val w = e.tests(0); assertEquals(List("W"), w.params)
    val a = e.tests(1); assertEquals(List("A"), a.params)
    val ab = e.tests(2); assertEquals(List("AB"), ab.params)

    assertEngineMatches(e,
      Right(EngineNode(because = List(Left("A")), scenarioThatCausedNode = a, inputs = List("A"),
        yes = Right(EngineNode(because = List(Left("B")), scenarioThatCausedNode = ab, inputs = List("AB"),
          yes = Left(CodeAndScenarios("Y", List(ab))),
          no = Left(CodeAndScenarios("X", List(a))))),
        no = Left(CodeAndScenarios("Z", List(w))))))
  }

  it should "throw ScenarioConflictException if it cannot decide between two scenarios" in {
    val bldr = builderWithDefault.
      scenario("AB").because("A").expected("X").
      scenario("AB").because("B").expected("Y");

    val e = evaluating { bldr.build } should produce[ScenarioConflictException]

  }

  it should "Throw ScenarioConflictingWithDefaultException if scenario matches root condition and comes to wrong conclusion" in {
    val bldr = builderWithDefault.
      scenario("AB").expected("X")
    evaluating {
      bldr.build
    } should produce[ScenarioConflictingWithDefaultException]

  }

  it should "throw ScenarioConflictingWithoutBecause if no because and doesnt come to correct result" in {
    val b = builderWithDefault.
      scenario("A").because("A").expected("X").
      scenario("AB").expected("Y")
    evaluating { b.build } should produce[ScenarioConflictingWithoutBecauseException]
  }

}