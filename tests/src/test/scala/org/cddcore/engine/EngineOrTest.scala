package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder._

abstract class EngineOrTest[Params, R, B <: Builder[Params, R, R, B, E], E <: EngineTools[Params, R]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, R, R, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  protected def expectedOrRuleErrorMessage: String

  protected def checkOrRule(expected: (Scenario[Params, R], Scenario[Params, R]) => DecisionTreeNode[Params, R]) = {
    import ReportableHelper._
    val e = build.asInstanceOf[EngineFromTests[Params, R]]
    val sa = e.asRequirement.scenarios(0)
    val sb = e.asRequirement.scenarios(1)
    assertEquals(expected(sa, sb), e.tree.root)

    assertEquals(result("W"), e.applyParams(params("A")))
    assertEquals(result("W"), e.applyParams(params("AB")))

    evaluating { e.applyParams(params("C")) } should produce[UndecidedException]
    e
  }

  "A Engine with an if then rule" should "Add 'or' to the existing parent if new scenario with same result but different reason (no code blocks)" in {
    scenario("A"); expected("W"); because("A")
    scenario("AB"); expected("W"); because("B")

    val e = checkOrRule((sa, sb) => dec(List(sa, sb), conc(sa, sb), defaultRoot))
    assertEquals(result("W"), e.applyParams(params("B")))
  }

  
  it should "not add an or if there is a code block specified in first scenario" in {
    scenario("A"); expected("W"); because("A"); code("W")
    scenario("AB"); expected("W"); because("B")

    val e = checkOrRule((sa, sb) => dec(sa, dec(sb, conc(sb), conc(sa)), defaultRoot))
    evaluating { e.applyParams(params("B")) } should produce[UndecidedException]
  }

  it should "not add an or if there is a code block specified in second scenario" in {
    scenario("A"); expected("W"); because("A");
    scenario("AB"); expected("W"); because("B"); code("W")

    val e = checkOrRule((sa, sb) => dec(sa, dec(sb, conc(sb), conc(sa)), defaultRoot))
    evaluating { e.applyParams(params("B")) } should produce[UndecidedException]
  }
  
  

  "An engine" should "Add scenario to root if adding with same conclusion, different reason" in {
    scenario("A");
    expected("W");

    scenario("B")
    because("B") //The because is totally redundant in this case. As everything gets to be W
    expected("W")

    val e = build.asInstanceOf[EngineFromTests[Params, R]]
    val sa = s("A", expected = "W")
    val sb = s("B", expected = "W", because = "B")
    assertEquals(conc(sa, sb), e.tree.root)
  }

  it should "not allow a scenario in the else clause of the parent to be broken" in {
    scenario("-"); expected("none")
    scenario("A"); expected("x"); because("A") //if a then x else none
    scenario("AC"); expected("y"); because("AC") // if a then (if ac then y else x) else none
    scenario("ACD"); expected("y"); because("A") //would make if  a then (if <ac or a> then y else x) else none this obviously messes up scenario a

    val e = evaluating { build } should produce[ScenarioCausingProblemWithOrRuleException]
    assertEquals(expectedOrRuleErrorMessage, e.getMessage())
    assertEquals(s("ACD", expected = "y", because = "A"), e.scenario)
    assertEquals(List(s("A", expected = "x", because = "A")), e.scenariosThatWouldBeBroken)
  }

}

abstract class EngineOr1Test[P, R] extends EngineOrTest[P, R,  Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineOr2Test[P1, P2, R] extends EngineOrTest[(P1, P2),  R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineOr3Test[P1, P2, P3, R] extends EngineOrTest[(P1, P2, P3),  R,  Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineOrStringStringTest extends EngineOr1Test[String, String] with StringStringTest {
  val expectedOrRuleErrorMessage = "" +
    "The scenario you added already came to the correct conclusion. \n" +
    "As well as that it had a because clause, and if the because clause was added, other scenario(s) that as already been added would now come to the wrong conclusion\n" +
    "Scenario being added:\n" +
    "Scenario:\n" +
    "Scenario(ACD,None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(y)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "ACD\n" +
    "\n------------------------------------------------------------------------------------\n" +
    "Scenarios that would be brokenScenario:\n" +
    "Scenario(A,None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(x)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "A"
}

@RunWith(classOf[JUnitRunner])
class EngineOrStringStringStringTest extends EngineOr2Test[String, String, String] with StringStringStringTest {
  val expectedOrRuleErrorMessage = "" +
    "The scenario you added already came to the correct conclusion. \n" +
    "As well as that it had a because clause, and if the because clause was added, other scenario(s) that as already been added would now come to the wrong conclusion\n" +
    "Scenario being added:\n" +
    "Scenario:\n" +
    "Scenario((ACD,ACD),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(y)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "(ACD,ACD)\n" +
    "\n------------------------------------------------------------------------------------\n" +
    "Scenarios that would be brokenScenario:\n" +
    "Scenario((A,A),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(x)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "(A,A)"
}

@RunWith(classOf[JUnitRunner])
class EngineOrLensStringStringStringStringTest extends EngineOr3Test[String, String, String, String] with StringStringStringStringTest {
  val expectedOrRuleErrorMessage = "" +
    "The scenario you added already came to the correct conclusion. \n" +
    "As well as that it had a because clause, and if the because clause was added, other scenario(s) that as already been added would now come to the wrong conclusion\n" +
    "Scenario being added:\n" +
    "Scenario:\n" +
    "Scenario((ACD,ACD,ACD),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(y)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "(ACD,ACD,ACD)\n" +
    "\n------------------------------------------------------------------------------------\n" +
    "Scenarios that would be brokenScenario:\n" +
    "Scenario((A,A,A),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(x)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "(A,A,A)"

}
