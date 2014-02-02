package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine._

@RunWith(classOf[JUnitRunner])
class BuilderEngineUseCaseScenarioTests extends AbstractTest {
  implicit def toFullEngine[R, FullR](e: Engine) = e.asInstanceOf[EngineFull[R, FullR]]
  "A builder" should "allow a useCase to be added, and set the depth to 1" in {
    val b = Engine[Int, String]().useCase("uc1")
    val e = b.build

    val ucb = b.builderData.children(0).asInstanceOf[RequirementAndHolder]
    val uce = e.children(0).asInstanceOf[RequirementAndHolder]
    assertEquals(ucb, uce)
    assertEquals(1, b.builderData.children.size)
    assertEquals(1, e.children.size)
    assertEquals(1, b.builderData.depth)
    assertEquals(ucb, b.buildTarget)
  }

  it should "allow a scenario to be added under a use case" in {
    val bu = Engine[Int, String]().useCase("uc1")
    val bs = bu.scenario(0)

    val b = bs.expected("z")
    val e = b.build

    val ucb = b.builderData.head.asInstanceOf[RequirementAndHolder]
    val sb = ucb.children.head.asInstanceOf[Test]

    val uce = e.children(0).asInstanceOf[RequirementAndHolder]
    val se = uce.children.head.asInstanceOf[Test]

    assertEquals(1, bu.builderData.depth)
    assertEquals(2, bs.builderData.depth)
    assertEquals(2, b.builderData.depth)
    assertEquals(sb, b.buildTarget)

  }

  it should "allow another useCase to be added" in {
    val bu = Engine[Int, String]().useCase("uc1")
    val b = bu.useCase("uc2");
    val e = b.build
    assertEquals(1, bu.builderData.depth)
    assertEquals(1, b.builderData.depth)
    val useCases = b.builderData.allReversed(classOf[RequirementAndHolder])
    assertEquals(List("uc1", "uc2"), useCases.map(_.titleString))
    val uc1 = useCases(0)
    val uc2 = useCases(1)
    assertEquals(List(), uc1.children)
    assertEquals(List(), uc2.children)
    assertEquals(List(uc2, uc1), b.builderData.children)
    assertEquals(uc2, b.buildTarget)
  }

  it should "allow usecase / scenario / usecase  to be added" in {
    val b1 = Engine[Int, String]().useCase("uc1").scenario(1).expected("x")
    val b2 = b1.useCase("uc2")
    val useCases = b2.builderData.allReversed(classOf[RequirementAndHolder])
    assertEquals(List("uc1", "uc2"), useCases.map(_.titleString))
    val uc1 = useCases(0)
    val uc2 = useCases(1)
    assertEquals(uc2, b2.buildTarget)
  }

  it should "allow usecase / scenario / usecase  /scenario to be added" in {
    val b1 = Engine[Int, String]().useCase("uc1").scenario(1).expected("x")
    val b2 = b1.useCase("uc2")
    val b3 = b2.scenario(2).expected("x")
    val b4 = b3.useCase("uc3").scenario(3).expected("x")
    val useCases = b4.builderData.allReversed(classOf[RequirementAndHolder])
    assertEquals(List("uc1", "uc2", "uc3"), useCases.map(_.titleString))
  }

  it should "allow scenarios without usecases to be added" in {
    val e = Engine[Int, String]().
      scenario(1).expected("one").because((x: Int) => x == 1).
      scenario(2).expected("two").because((x: Int) => x == 2).
      build
    assertEquals("one", e(1))
    assertEquals("two", e(2))
  }

  "An engine with a child engine" should "allow an engine to be specified" in {
    val e = Engine.folding[Int, String, String]((acc, s) => acc, "").childEngine("engineDescription").build;
  }

  it should "allow use cases to be specified" in {
    val e = Engine.folding[Int, String, String]((acc, s) => acc, "").childEngine("engineDescription").
      useCase("uc1").useCase("uc2").
      build;
    val useCases = e.all(classOf[UseCase])
    assertEquals(List("uc1", "uc2"), useCases.map(_.titleString))
    val uc1 = useCases(0)
    val uc2 = useCases(1)
  }

  it should "allow scenarios to be specifed" in {
    val e = Engine.foldList[Int, String].
      childEngine("engineDescription").
      scenario(1).expected("one").because((x: Int) => x == 1).
      scenario(2).expected("two").because((x: Int) => x == 2).
      build
    assertEquals(List("one"), e(1))
    assertEquals(List("two"), e(2))
  }

  it should "have its scenarios" in {
    val bce = Engine.folding[Int, String, String]((acc, s) => acc, "").childEngine("engineDescription");
    val b = bce.
      scenario(11).expected("1one").because((x: Int) => x == 11).
      scenario(12).expected("1two").because((x: Int) => x == 12);

    val engines = b.builderData.all(classOf[ChildEngine[_]]);
  }

  it should "only have its scenarios" in {
    val e = Engine.folding[Int, String, String]((acc, s) => acc, "").childEngine("ed1").
      scenario(11).expected("1one").because((x: Int) => x == 11).
      scenario(12).expected("1two").because((x: Int) => x == 12).
      childEngine("ed2").
      scenario(21).expected("2one").because((x: Int) => x == 21).
      scenario(22).expected("2two").because((x: Int) => x == 22).
      build

    val engines = e.childEngines
    assertEquals(List("ed1", "ed2"), engines.map(_.titleString))
    assertEquals(List("11", "12"), engines(0).tests.map(_.titleString))
    assertEquals(List("21", "22"), engines(1).tests.map(_.titleString))

  }

  it should "allow use cases and scenarios under it in" in {
    val b = Engine.folding[Int, String, String]((acc, s) => acc, "").childEngine("ed1").
      useCase("uc11").
      scenario(111).expected("1one").because((x: Int) => x == 111).
      scenario(112).expected("1two").because((x: Int) => x == 112).
      useCase("uc12").
      scenario(121).expected("1one").because((x: Int) => x == 121).
      scenario(122).expected("1two").because((x: Int) => x == 122)
    val e = b.
      childEngine("ed2").
      scenario(21).expected("2one").because((x: Int) => x == 21).
      scenario(22).expected("2two").because((x: Int) => x == 22).
      build

    val engines = e.childEngines
    assertEquals(List("ed1", "ed2"), engines.map(_.titleString))
    val uc1s = engines(0).all(classOf[UseCase])
    val uc2s = engines(1).all(classOf[UseCase])
    assertEquals(List("uc11", "uc12"), uc1s.map(_.titleString))
    assertEquals(List(), uc2s.map(_.titleString))
    assertEquals(List("111", "112", "121", "122"), engines(0).tests.map(_.titleString))
    assertEquals(List("21", "22"), engines(1).tests.map(_.titleString))
    assertEquals(List("111", "112"), uc1s(0).all(classOf[Test]).map(_.titleString))
    assertEquals(List("121", "122"), uc1s(1).all(classOf[Test]).map(_.titleString))
  }

  it should "not allow a child engine after a use case or a scenario" in {
    evaluating { Engine[Int, String]().useCase("").childEngine("ed1") } should produce[CanAddChildEngineAfterUseCaseOrScenarioException]
    evaluating { Engine[Int, String]().scenario(0).childEngine("ed1") } should produce[CanAddChildEngineAfterUseCaseOrScenarioException]
    evaluating { Engine[Int, String]().useCase("").scenario(0).childEngine("ed1") } should produce[CanAddChildEngineAfterUseCaseOrScenarioException]
  }

}