package org.cddcore.engine

import org.junit.runner.RunWith

import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.engine.builder._
import EngineTools._

abstract class EngineThrowingExceptionTest[Params, R, FullR, B <: Builder[Params, R, R, B, E], E <: EngineTools[Params, R]] extends BuilderTest[Params, R, R, B, E] with DecisionTreeBuilderForTests[Params, R] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[Params, R]]
  implicit def toEngineFromTests[Params, R](x: EngineTools[Params, R]) = x.asInstanceOf[EngineFromTests[Params, R]]
  implicit def toResult(x: String) = result(x)
  implicit def toParams(x: String) = params(x)

  implicit def toSome[X](x: X) = Some(x)

  class UniqueExceptionForTest extends Exception
  class UniqueExceptionForTest1 extends UniqueExceptionForTest

  "A blank engine" should "have an empty exceptionsItCanThrow list" in {
    val e = build
    assertEquals(Set(), e.exceptionsItCanThrow)
  }

  "An engine" should "have any specified exceptions added to its exceptionsItCanThrow list" in {
    scenario("A"); expectException(new UniqueExceptionForTest); codeThrows(new UniqueExceptionForTest1)
    val e = build
    assertEquals(Set( classOf[UniqueExceptionForTest]), e.exceptionsItCanThrow)
  }

  it should "throw the specified exception" in {
    scenario("A"); expectException(new UniqueExceptionForTest); codeThrows(new UniqueExceptionForTest)
    val e = build
    evaluating(e.applyParams(params("a"))) should produce[UniqueExceptionForTest]
  }

  it should "throw a child of the specified exception" in {
    scenario("A"); expectException(new UniqueExceptionForTest); codeThrows(new UniqueExceptionForTest1)
    val e = build
    evaluating(e.applyParams(params("a"))) should produce[UniqueExceptionForTest1]
  }
  it should "throw failed to execute exception is an unexpected exception is thrown" in {
    scenario("A"); expectException(new UniqueExceptionForTest); codeThrows(new UniqueExceptionForTest)
    val e = build
    evaluating(e.applyParams(params("a"))) should produce[UniqueExceptionForTest]
  }
  "A blank engine" should "UndecidedException, which should have the engine and params recorded" in {
    val e = build
    val ex = evaluating(e.applyParams(params("a"))) should produce[UndecidedException]
    assertEquals(params("a"), ex.params)
    assertEquals(e, ex.engine)
  }
}
abstract class EngineThrowingException1Test[P, R, FullR] extends EngineThrowingExceptionTest[P, R,  FullR, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R] {
}
abstract class EngineThrowingException2Test[P1, P2, R, FullR] extends EngineThrowingExceptionTest[(P1, P2),  R, FullR, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R] {
}
abstract class EngineThrowingException3Test[P1, P2, P3, R, FullR] extends EngineThrowingExceptionTest[(P1, P2, P3),  R,  FullR, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R] {
}

@RunWith(classOf[JUnitRunner])
class EngineThrowingStringStringTest extends EngineThrowingException1Test[String, String, String] with StringStringTest {

}

@RunWith(classOf[JUnitRunner])
class EngineThrowingStringStringStringTest extends EngineThrowingException2Test[String, String, String, String] with StringStringStringTest {

}

@RunWith(classOf[JUnitRunner])
class EngineThrowingCacheStringStringStringStringTest extends EngineThrowingException3Test[String, String, String, String, String] with StringStringStringStringTest {

}