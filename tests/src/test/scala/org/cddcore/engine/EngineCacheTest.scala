package org.cddcore.engine

import org.junit.runner.RunWith

import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.engine.builder._
import EngineTools._

abstract class EngineCacheTest[Params, BFn, R, RFn, FullR, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]] extends BuilderTest[Params, BFn, R, RFn, R, B, E] with DecisionTreeBuilderForTests[Params, BFn, R, RFn] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[Params, BFn, R, RFn]]
  implicit def toEngineFromTests[Params, BFn, R, RFn](x: EngineTools[Params, BFn, R, RFn]) = x.asInstanceOf[EngineFromTests[Params, BFn, R, RFn]]
  implicit def toResult(x: String) = result(x)
  implicit def toParams(x: String) = params(x)

  implicit def toSome[X](x: X) = Some(x)

  def buildAndCached: (Engine, Engine)
  def cached: Engine = buildAndCached._2
  def evaluateCached(engine: Engine, params: Params): FullR

  s"A cached $builderName " should "return the delegate properites" in {
    val (engine, cached) = buildAndCached
    assertEquals(engine.asRequirement, cached.asRequirement)
    assertEquals(engine.evaluator, cached.evaluator)
    assertEquals(engine.titleString, cached.titleString)
    assertEquals(engine.ldp, cached.ldp)
    assert(engine.buildExceptions.eq(cached.buildExceptions))
  }

  it should "give the same results for a blank engine" in {
    val engine = cached
    evaluating { evaluateCached(engine, params("A")) } should produce[UndecidedException]
  }

  it should "give same results for populated engine" in {
    scenario("A"); expected("X")
    scenario("B"); expected("Y"); matchOn("B", "Y")
    val e = build
    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("Y"), e.applyParams("B"))
    assertEquals(result("Y"), e.applyParams("AB"))
    assertEquals(result("X"), e.applyParams("ACD"))
    
    assertEquals(result("X"), e.applyParams("A"))
    assertEquals(result("Y"), e.applyParams("B"))
    assertEquals(result("Y"), e.applyParams("AB"))
    assertEquals(result("X"), e.applyParams("ACD"))
  }

}

abstract class EngineCache1Test[P, R, FullR] extends EngineCacheTest[P, (P) => Boolean, R, (P) => R, FullR, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R] {
  implicit def toEngine(e: Engine) = e.asInstanceOf[Engine1[P, R, FullR]]
  def buildAndCached = { val e = build; (e, e.cached) }
  def evaluateCached(engine: Engine, params: P): FullR = engine(params)
}
abstract class EngineCache2Test[P1, P2, R, FullR] extends EngineCacheTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R] {
  implicit def toEngine(e: Engine) = e.asInstanceOf[Engine2[P1, P2, R, FullR]]
  def buildAndCached = { val e = build; (e, e.cached) }
  def evaluateCached(engine: Engine, params: (P1, P2)): FullR = engine(params._1, params._2)
}
abstract class EngineCache3Test[P1, P2, P3, R, FullR] extends EngineCacheTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R] {
  implicit def toEngine(e: Engine) = e.asInstanceOf[Engine3[P1, P2, P3, R, FullR]]
  def buildAndCached = { val e = build; (e, e.cached) }
  def evaluateCached(engine: Engine, params: (P1, P2, P3)): FullR = engine(params._1, params._2, params._3)
}

@RunWith(classOf[JUnitRunner])
class EngineCacheStringStringTest extends EngineCache1Test[String, String, String] with StringStringTest {

}

@RunWith(classOf[JUnitRunner])
class EngineCacheStringStringStringTest extends EngineCache2Test[String, String, String, String] with StringStringStringTest {

}

@RunWith(classOf[JUnitRunner])
class EngineCacheStringStringStringStringTest extends EngineCache3Test[String, String, String, String, String] with StringStringStringStringTest {

}
