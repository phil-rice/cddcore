package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineThrowingExceptionTest extends EngineStringStringTests {

  "An Engine" should "throw an exception" in {
    val builder = builderWithUseCase.scenario("e", "Throws Exception").
      expectException(new RuntimeException("with some message"))
    evaluating { builder.build } should produce[ExceptionWithoutCodeException]
  }

  it should "Allow an excepted result to be an exception" in {
    val engine = builderWithUseCase.scenario("e", "Throws Exception").
      code((s: String) => throw new RuntimeException("with some message")).
      expectException(new RuntimeException).
      build
    val e = evaluating { engine("") } should produce[RuntimeException]
    assertEquals("with some message", e.getMessage())
  }

  it should "throw exception if the code block doesn't throw an exception" in {
    val builder = builderWithUseCase.scenario("e", "Throws Exception").
      code((s: String) => "").
      expectException(new RuntimeException)
    val e = evaluating { builder.build } should produce[ScenarioResultException]
    assert(e.getMessage().contains("Expected: throws RuntimeException"))
  }

  it should "throw exception if the code block doesn't throw correct exception" in {
    val builder = builderWithUseCase.scenario("e", "Throws Exception").
      code((s: String) => throw new RuntimeException("with some message")).
      expectException(new IllegalStateException)
    val e = evaluating { builder.build } should produce[ScenarioResultException]
  }

}