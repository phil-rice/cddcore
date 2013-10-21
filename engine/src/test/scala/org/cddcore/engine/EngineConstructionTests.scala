package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringTest extends EngineStringStringTests {

  val wCString = "Z:W\n"

  val w_aCString = "if(hA)\n" +
    " XA:A\n" +
    "else\n" +
    " Z:W\n"

  val w_b_aCString = "if(hA)\n" +
    " XA:A\n" +
    "else\n" +
    " if(hB)\n" +
    "  XB:B\n" +
    " else\n" +
    "  Z:W\n"

  val w_ab_b_aCString = "if(hA)\n" +
    " if(hAB)\n" +
    "  XAB:AB\n" +
    " else\n" +
    "  XA:A\n" +
    "else\n" +
    " if(hB)\n" +
    "  XB:B\n" +
    " else\n" +
    "  Z:W\n"

  it should "return the aggregate of the toString of an engine created from the scenarios, one after another" in {
    val bldr = builderWithDefault.
      scenario("A").because(becauseA).expected("XA").
      scenario("B").because(becauseB).expected("XB").
      scenario("AB").because(becauseAB).expected("XAB")
    val e = bldr.build
    val cs = e.constructionString
    assertEquals("Adding Scenario(, W, because=, expected=Z)\n" + wCString + "\n" +
      "Adding Scenario(, A, because=hA, expected=XA)\n" + w_aCString + "\n" +
      "Adding Scenario(, B, because=hB, expected=XB)\n" + w_b_aCString + "\n" +
      "Adding Scenario(, AB, because=hAB, expected=XAB)\n" + w_ab_b_aCString, cs)
  }

  "An engine's increasingScenariosList method" should "return a list of increasing numbers of scenarios" in {
    val bldr = builderWithDefault.
      scenario("A").because(becauseA).expected("XA").
      scenario("B").because(becauseB).expected("XB").
      scenario("AB").because(becauseAB).expected("XAB")
    val e = bldr.build
    val w = e.scenarios(0); assertEquals(List("W"), w.params)
    val a = e.scenarios(1); assertEquals(List("A"), a.params)
    val b = e.scenarios(2); assertEquals(List("B"), b.params)
    val ab = e.scenarios(3); assertEquals(List("AB"), ab.params)

    val actual = e.increasingScenariosList(e.scenarios)
    val expected = List(List(ab, b, a, w), List(b, a, w), List(a, w), List(w))
    assertEquals(expected, actual)
  }

  "An engine's buildRoot method" should "return a node that represents the scenarios" in {
    val bldr = builderWithDefault.
      scenario("A").because(becauseA).expected("XA").
      scenario("B").because(becauseB).expected("XB").
      scenario("AB").because(becauseAB).expected("XAB")
    val e = bldr.build
    val w = e.scenarios(0); assertEquals(List("W"), w.params)
    val a = e.scenarios(1); assertEquals(List("A"), a.params)
    val b = e.scenarios(2); assertEquals(List("B"), b.params)
    val ab = e.scenarios(3); assertEquals(List("AB"), ab.params)

    val (actual, seMap) = e.buildRoot(e.defaultRoot, List(w, a, b, ab))
    assertEngineMatches(e, actual)
    assertEquals(0, seMap.size)
  }

}