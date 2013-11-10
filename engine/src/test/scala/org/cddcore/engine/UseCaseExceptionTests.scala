package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class UseCaseExpectedTests extends EngineStringStringTests {

  "A Builder" should "add an expected straight after the usecase declaration to the use case" in {
    val b1 = builderWithUseCase.expected("X")
    val useCase = b1.builderData.children(0).asInstanceOf[UseCase]
    assertEquals(Some(ROrException[String]("X")), useCase.expected)
  }
  
  "A Builder" should "not add an expected straight after a scenario  to the use case" in {
    val b1 = builderWithUseCase.scenario("a").expected("X")
    val useCase = b1.builderData.children(0).asInstanceOf[UseCase]
    assertEquals(None, useCase.expected)
  }

  "An engine" should "Use useCase expected if no expected is given" in {
    val e = builderWithUseCase.expected("X").
      scenario("A").because("A").
      scenario("B").because("B").
      scenario("C").expected("Y").because("C").build
    assertEquals("X", e("A"))
    assertEquals("X", e("B"))
    assertEquals("Y", e("C"))
  }

}