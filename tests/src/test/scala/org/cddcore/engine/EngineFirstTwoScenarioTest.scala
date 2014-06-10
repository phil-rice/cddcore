package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder._

abstract class EngineFirstTwoScenarioTest[Params, R, B <: Builder[Params, R, R, B, E], E <: EngineTools[Params, R]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, R, R, B, E] {
  implicit def toDecisionTreeDecisionTree[Params, R](x: EngineTools[Params, R]) =
    x.asInstanceOf[EngineFromTests[Params, R]].tree
  implicit def toEngineFromTests[Params, R](x: EngineTools[Params, R]) =
    x.asInstanceOf[EngineFromTests[Params, R]]
  implicit def toSome[X](x: X) = Some(x)
  implicit def toResult(x: String) = result(x)
  implicit def toParams(x: String) = params(x)

  builderName should "allow the first scenario not to have a because, and become the default value" in {
    scenario("A")
    expected("X")
    val e = build
    assertEquals(conc(s("A", expected = "X")), e.root)

    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("X"), e.applyParams("B"))
  }

  it should "allow that even if the scenario throws an exception" in {
    val e = new RuntimeException
    scenario("A"); expectException(e); codeThrows(e)
    val engine = build
    val ex = evaluating {engine.applyParams("A")} should produce[RuntimeException]
    assertEquals(e, ex)

  }

  it should " allow the first scenario not to have a because, and become the default value when we add a second scenario " in {
    scenario("A"); expected("X")
    scenario("B"); expected("Y"); because("B")
    val e = build

    val s1 = s("A", expected = "X")
    val s2 = s("B", expected = "Y", because = "B")
    assertEquals(dec(s2, conc(s2), conc(s1)), e.root)
    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("Y"), e.applyParams("B"))
    assertEquals(result("Y"), e.applyParams("AB"))
    assertEquals(result("X"), e.applyParams("ACD"))
  }

  it should "allow the use of the matchOn syntax" in {
    scenario("A"); expected("X")
    scenario("B"); expected("Y"); matchOn("B", "Y")
    val e = build
    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("Y"), e.applyParams("B"))
    assertEquals(result("Y"), e.applyParams("AB"))
    assertEquals(result("X"), e.applyParams("ACD"))
  }

  it should "allow expectedAndCode" in {
    code("Q")
    scenario("A"); expectedAndCode("X"); because("A")
    scenario("B"); expectedAndCode("Y"); because("B")
    val e = build
    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("Y"), e.applyParams("B"))
    assertEquals(result("X"), e.applyParams("AB"))
    assertEquals(result("X"), e.applyParams("ACD"))
    assertEquals(result("Q"), e.applyParams("123"))
  }

  it should "use the priority" in {
    scenario("B"); expected("Y"); because("B")
    scenario("A"); expected("X"); update(_.priority(1))
    val e = build

    val s1 = s("A", expected = "X", priority = 1)
    val s2 = s("B", expected = "Y", because = "B")
    assertEquals(dec(s2, conc(s2), conc(s1)), e.root)
    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("Y"), e.applyParams("B"))
    assertEquals(result("Y"), e.applyParams("AB"))
    assertEquals(result("X"), e.applyParams("ACD"))
  }

  it should "allow use cases to be specified with a title and a description " in {
    update(_.useCase("title1", "description"))
    assertEquals(EngineDescription[Params, R](nodes = List(UseCase[Params, R](
      title = Some("title1"),
      description = Some("description")))), currentBuilder.nodes.head)
  }

  it should "still throw an UndecidedException if a because clause is given by the first scenario when parameters don't match the because clause" in {
    scenario("A")
    expected("X")
    because("A")
    val e = build

    val s1 = s("A", expected = "X", because = "A")
    assertEquals(dec(s1, conc(s1), defaultRoot), e.root)

    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("X"), e.applyParams("AB"))
    evaluating { e.applyParams("B") } should produce[UndecidedException]
  }

  val doc = new Document(Some("doc"))
  val ref1 = Reference("ref1")
  val ref2 = Reference("ref2", doc)

  it should "add the references to the engine" in {
    update(_.reference("ref1").reference("ref2", doc))
    val e = build
    assertEquals(Set(ref1, ref2), e.asRequirement.references)
  }

  it should "add the references to the usecases" in {

    update(_.useCase("").reference("ref1").reference("ref2", doc))
    val e = build
    import ReportableHelper._
    val uc = e.asRequirement.useCases(0)
    assertEquals(Set(ref1, ref2), uc.references)
    assertEquals(List(doc), e.asRequirement.documents)
  }
  it should "add the references to the scenarios" in {
    scenario("A")
    expected("Something")
    update(_.reference("ref1").reference("ref2", doc))
    val e = build
    import ReportableHelper._
    val s = e.asRequirement.scenarios(0)
    assertEquals(Set(ref1, ref2), s.references)
    assertEquals(List(doc), e.asRequirement.documents)
  }

  it should "throw a DuplicateScenarioException if the same scenario is added" in {
    scenario("A")
    evaluating { scenario("A") } should produce[DuplicateScenarioException]
  }

  it should "let a second scenario be added to the root is it's just an assertion " in {
    scenario("A"); expected("X")
    scenario("B"); expected("X")
    val e = build

    assertEquals(conc(s("A", expected = "X"), s("B", expected = "X")), e.root)
  }

  it should "Throw NoExpectedException if scenario doesnt have expected" in {
    scenario("A")
    evaluating { build } should produce[NoExpectedException]
  }

  it should "Throw ScenarioBecauseException if because is not true in scenario" in {
    scenario("A")
    evaluating { because("X") } should produce[ScenarioBecauseException]
  }

  it should "allow exceptException " in {
    val ex1 = new RuntimeException
    val ex2 = new RuntimeException
    scenario("A"); expectException(ex1); codeThrows(ex2)
    val e = build
    val actual = evaluating { e.applyParams("A") } should produce[RuntimeException]
    assertEquals(ex2, actual)
  }

  it should "throw ScenarioShouldHaveCodeIfExpectsException if there is exceptException without a code block" in {
    val ex1 = new RuntimeException
    scenario("A"); expectException(ex1);
    evaluating { build } should produce[ScenarioShouldHaveCodeIfExpectsException]
  }

  it should "throw CodeDoesntProduceExpectedException if there is a code block that produces the wrong expected" in {
    scenario("A")
    expected("X")
    code("Y")
    evaluating { build } should produce[CodeDoesntProduceExpectedException]

  }

}

abstract class EngineFirstTwoScenario1Test[P, R] extends EngineFirstTwoScenarioTest[P, R,  Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineFirstTwoScenario2Test[P1, P2, R] extends EngineFirstTwoScenarioTest[(P1, P2),  R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineFirstTwoScenario3Test[P1, P2, P3, R] extends EngineFirstTwoScenarioTest[(P1, P2, P3),  R,  Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioStringStringTest extends EngineFirstTwoScenario1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioStringStringStringTest extends EngineFirstTwoScenario2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioStringStringStringStringTest extends EngineFirstTwoScenario3Test[String, String, String, String] with StringStringStringStringTest

