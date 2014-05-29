package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder._
import org.cddcore.utilities.NestedHolder
import ReportableHelper._

abstract class EngineInTestModeTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] with ConflictMessages {
  implicit def toSome[X](x: X) = Some(x)

  def checkExceptions(doIt: => Unit, scenarioClassMessage: (Scenario[Params, BFn, R, RFn], List[(Class[_], Option[String])])*) = {
    val messages = scenarioClassMessage.toList
    val e = Engine.test {
      doIt
      build
    }
    val s = e.asRequirement.scenarios
    val n : NestedHolder[BuilderNode[Params,BFn, R, RFn]] = e.asRequirement.asInstanceOf[BuilderNodeAndHolder[Params, BFn,R, RFn] ]
    val exceptions = e.buildExceptions.toMap[BuilderNode[Params,BFn, R, RFn]](n)
    assertEquals(messages.size, exceptions.size)
    for ((s, list) <- messages) {
      val actual = exceptions(s)
      assertEquals(list.size, actual.size)
      for ((e, (c, msg)) <- actual.zip(list)) {
        assertEquals(c, e.getClass)
        msg match {
          case Some(m) => assertEquals(m, e.getMessage())
          case _ =>
        }
      }
    }
  }
  builderName should "store  CannotDefineTitleTwiceException if the title has already been set in test mode" in {
    checkExceptions({
      scenario("A"); expected("X")
      update(_.title("X"))
      update(_.title("X"))
    },
      (s("A", title = "X", expected = "X"), List((classOf[CannotDefineTitleTwiceException], None))))
  }

  builderName should "store ScenarioConflictingWithDefaultAndNoBecauseException if comes to different conclusion when there is decision node in test mode" in {

    checkExceptions({ scenario("A"); because("A"); expected("X"); scenario("B"); code("Z"); expected("Z") },
      (s("B", code = resultCodeHolder("Z"), expected = "Z"),
        List((classOf[ScenarioConflictingWithDefaultAndNoBecauseException], expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode))))
  }

  it should "store ScenarioConflictingWithoutBecauseException if there is a non default conclusion and no because clause in the scenario in test mode" in {
    checkExceptions({
      scenario("A"); because("A"); expected("X");
      scenario("AB"); expected("Y")
    },
      (s("AB", expected = "Y"), List(
        (classOf[ScenarioConflictingWithoutBecauseException], expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode))))
  }

  it should "store ScenarioConflictAndBecauseNotAdequateException if the scenario being added has a different tconclusion and the because isn't good enough to differentiate it from the other scenarios in the conclusion with just root in test mode" in {
    checkExceptions({
      scenario("AB"); expected("X")
      scenario("AC"); expected("X")
      scenario("BC"); expected("Y"); because("B")
    },
      (s("BC", expected = "Y", because = "B"), List((classOf[ScenarioConflictAndBecauseNotAdequateException], expectedMessageFoBecauseNotAdequate))))
  }

  it should "store  CannotDefineDescriptionTwiceException if the description has already been set in test mode" in {
    checkExceptions({
      scenario("A"); expected("X")
      update(_.description("X"))
      update(_.description("X"))
    },
      (s("A", description = "X", expected = "X"), List((classOf[CannotDefineDescriptionTwiceException], None))))
  }
  it should "store  CannotDefinePriorityTwiceException if the priority has already been set in test mode" in {
    checkExceptions({
      scenario("A"); expected("X")
      update(_.priority(1))
      update(_.priority(1))
    },
      (s("A", priority = 1, expected = "X"), List((classOf[CannotDefinePriorityTwiceException], None))))
  }
  it should "store  CannotDefineBecauseTwiceException if the because has already been set in test mode" in {
    checkExceptions({
      scenario("A"); expected("X")
      because("A")
      because("A")
    },
      (s("A", expected = "X", because = "A"), List((classOf[CannotDefineBecauseTwiceException], None))))
  }

  it should "store  CannotDefineExpectedTwiceException if the expected has already been set in test mode" in {
    checkExceptions({
      scenario("A");
      expected("X")
      expected("X")
    },
      (s("A", expected = "X"), List((classOf[CannotDefineExpectedTwiceException], None))))
  }
  it should "store  CannotDefineCodeTwiceException if the code has already been set in test mode" in {
    checkExceptions({
      scenario("A"); expected("X")
      code("X")
      code("X")
    },
      (s("A", code = resultCodeHolder("X"), expected = "X"), List((classOf[CannotDefineCodeTwiceException], None))))
  }

  it should "store ScenarioBecauseException  if the because is not true in the scenario in test mode" in {
    checkExceptions({
      scenario("A"); expected("X")
      because("B")
    },
      (s("A", expected = "X"), List((classOf[ScenarioBecauseException], None))))
  }
  it should "store BecauseClauseException if an exception is thrown by the because in test mode" in {
    checkExceptions({
      val e = new RuntimeException
      scenario("A"); expected("X")
      becauseException(e)
    },
      (s("A", expected = "X"), List((classOf[BecauseClauseScenarioException], None))))
  }

}

abstract class EngineInTestMode1Test[P, R] extends EngineInTestModeTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineInTestMode2Test[P1, P2, R] extends EngineInTestModeTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineInTestMode3Test[P1, P2, P3, R] extends EngineInTestModeTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineInTestModeStringStringTest extends EngineInTestMode1Test[String, String] with StringStringTest with ConflictMessages1

@RunWith(classOf[JUnitRunner])
class EngineInTestModeStringStringStringTest extends EngineInTestMode2Test[String, String, String] with StringStringStringTest with ConflictMessages2

@RunWith(classOf[JUnitRunner])
class EngineInTestModeStringStringStringStringTest extends EngineInTestMode3Test[String, String, String, String] with StringStringStringStringTest with ConflictMessages3

