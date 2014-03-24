package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineOrTests extends EngineStringStringTests {

  "A Engine with an if then rule" should "Add 'or' to the existing parent if new scenario with same result but different reason" in {
    val b = builderWithDefault.
      scenario("A", "a").because("A").expected("W").
      scenario("AB", "ab").because("B").expected("W")
    val e = b.build
    val wScenario = e.tests(0)
    val aScenario = e.tests(1)
    val abScenario = e.tests(2)
    assertEngineMatches(e,
      Right(EngineNode(List("B", "A"), inputs = List("A"), yes = Left(CodeAndScenarios("W", List(abScenario, aScenario))),
        no = Left(CodeAndScenarios("Z", List(wScenario))),
        scenarioThatCausedNode = aScenario)))
    assertEquals("W", e("A"))
    assertEquals("W", e("AB"))
    assertEquals("W", e("B"))
  }

  "An engine" should "Add scenario to root if adding with same conclusion, different reason" in {
    val b = builderWithDefault.scenario("B").because("B").expected("Z") //The B is totally redundant in this case. As everything gets to be Z.
    val e1 = builderWithDefault.build
    val e2 = b.build
    val bScenario = e2.tests(1)
    assertEngineMatches(e1, Left(CodeAndScenarios("Z", List(defaultScenario))))
    assertEngineMatches(e2, Left(CodeAndScenarios("Z", List(bScenario, defaultScenario))))
    assertEquals("Z", e2("A"))
    assertEquals("Z", e2("Q"))
    assertEquals("Z", e2("B"))
  }

  it should "make an or rule even if several deep" in {
    val bldr = builderWithDefault.
      scenario("AB", "a").because("A").expected("X").
      scenario("AB", "b").because("B").expected("X");

    val e = bldr.build
    val w = e.tests(0); assertEquals(List("W"), w.params)
    val xBecauseA = e.tests(1); assertEquals("A", xBecauseA.becauseString)
    val xBecauseB = e.tests(2); assertEquals("B", xBecauseB.becauseString)

    assertEngineMatches(e,
      Right(EngineNode(List("B", "A"), inputs = List("AB"), yes = Left(CodeAndScenarios("X", List(xBecauseB, xBecauseA))),
        no = Left(CodeAndScenarios("Z", List(w))),
        scenarioThatCausedNode = xBecauseA)))
    assertEquals("X", e("A"))
    assertEquals("X", e("B"))
    assertEquals("Z", e("C"))
  }

  it should "not allow a scenario in the else clause of the parent to be broken" in {
    case class FourBooleans(a: Boolean, b: Boolean, c: Boolean, d: Boolean)
    implicit def toFourBooleans(x: (Boolean, Boolean, Boolean, Boolean)) = FourBooleans(x._1, x._2, x._3, x._4)
    Engine.logging = true
    val builder = Engine[FourBooleans, String]().
      scenario((false, false, false, false), "-").expected("none").
      scenario((true, false, false, false), "A").expected("a").because((w: FourBooleans) => w.a).
      scenario((true, false, true, false), "AC").expected("d").because((w: FourBooleans) => w.a && w.c).
      scenario((false, true, false, false), "B").expected("b").because((w: FourBooleans) => w.b).
      scenario((true, false, true, true), "ACD").expected("d").because((w: FourBooleans) => w.a && true)

    val e = evaluating { builder.build } should produce[EngineException]
    assertEquals(
      "The scenario you added already came to the correct conclusion. \n" +
        "As well as that it had a because clause, and if the because clause was added, another scenario that as already been added would now come to the wrong conclusion\n" +
        "Existing: A\n" +
        "  which used to come to a, would now come to d\n" +
        "Being Added: ACD\n" +
        "Detailed existing:\n" +
        "Scenario(A, FourBooleans(true,false,false,false), because=((w: FourBooleans) => w.a), expected=a)\n" +
        "Detailed of being Added:\n" +
        "Scenario(ACD, FourBooleans(true,false,true,true), because=((w: FourBooleans) => w.a.&&(true)), expected=d)", e.getMessage())
  }

}