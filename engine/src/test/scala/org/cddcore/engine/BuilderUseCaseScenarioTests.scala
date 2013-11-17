package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine._

@RunWith(classOf[JUnitRunner])
class BuilderUseCaseScenarioTests extends AbstractTest {

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
    val useCases = b.builderData.all(classOf[RequirementAndHolder])
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
    val useCases = b2.builderData.all(classOf[RequirementAndHolder])
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
    val useCases = b4.builderData.all(classOf[RequirementAndHolder])
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

}