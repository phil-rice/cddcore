package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder._
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicInteger

@RunWith(classOf[JUnitRunner])
abstract class EngineAssertionTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {

  "The assert method" should "have the parameters and result passed to them" in {
    var count = new AtomicInteger(0)
    scenario("A"); expected("X")
    assert((p, r) => { count.incrementAndGet(); assertEquals(params("A"), p); assertEquals(result("X"), r); true })
    Engine.test { build }
    assertEquals(1, count.get)

    build
    assertEquals(3, count.get) //assertion is called in post validation when not in test mode
  }

  it should "throw ExceptionThrownInAssertion if the assertion throws an exception" in {
    val e = new RuntimeException
    scenario("A"); expected("X")
    assert((params, r) => throw e)
    val ex = evaluating { build } should produce[ExceptionThrownInAssertion]
    assertEquals(e, ex.getCause())
  }

  it should "cause the build to fail with an AssertionException if they return false " in {
    scenario("A"); expected("X")
    assert((params, r) => false)
    evaluating { build } should produce[AssertionException]
  }

  "The assertMatch method" should "have the parameters and result passed to them" in {

    var count = new AtomicInteger(0)
    scenario("A"); expected("X")
    assertMatch { case (p, r) => { count.incrementAndGet(); assertEquals(params("A"), p); assertEquals(result("X"), r); true } }
    Engine.test { build }
    assertEquals(1, count.get)

    build
    assertEquals(3, count.get) //assertion is called in post validation when not in test mode
  }

  it should "throw ExceptionThrownInAssertion if the assertion throws an exception" in {
    val e = new RuntimeException
    scenario("A"); expected("X")
    assertMatch { case (params, r) => throw e }
    val ex = evaluating { build } should produce[ExceptionThrownInAssertion]
    assertEquals(e, ex.getCause())
  }

  it should "cause the build to fail with an AssertionException if 'not defined at' is false" in {
    scenario("A"); expected("X")
    assertMatch { case (params, r) if false => true }
    val ex = evaluating { build } should produce[AssertionException]
  }

  it should "cause the build to fail with an AssertionException if they return false " in {
    scenario("A"); expected("X")
    assertMatch { case (p, r) => false }
    evaluating { build } should produce[AssertionException]
  }

  "The assertException method" should "have the parameters and exception passed to them" in {
    var count = new AtomicInteger(0)
    val e1 = new RuntimeException
    val e2 = new RuntimeException
    scenario("A"); expectException(e1); codeThrows(e2)
    assertException { (p, e) => { count.incrementAndGet(); assertEquals(params("A"), p); assertEquals(e2, e); true } }
    Engine.test { build }
    assertEquals(1, count.get)
    build
    assertEquals(3, count.get) //assertion is called in post validation when not in test mode
  }

}

abstract class EngineAssertion1Test[P, R] extends EngineAssertionTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineAssertion2Test[P1, P2, R] extends EngineAssertionTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineAssertion3Test[P1, P2, P3, R] extends EngineAssertionTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineAssertionStringStringTest extends EngineAssertion1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineAssertionStringStringStringTest extends EngineAssertion2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineAssertionStringStringStringStringTest extends EngineAssertion3Test[String, String, String, String] with StringStringStringStringTest
