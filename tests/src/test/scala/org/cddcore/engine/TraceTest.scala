package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner
import org.cddcore.utilities.TraceItem

abstract class TraceTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  implicit def toParams(x: String) = params(x)
  implicit def toSimpleEngine(e: E) = e.asInstanceOf[EngineFromTests[Params, BFn, R, RFn]]

  scenario("A"); expected("X");
  val e1 = build
  resetBuilder

  scenario("A"); expected("Y");
  val e2 = build
  resetBuilder

  val exception = new RuntimeException
  scenario("A"); expectException(exception); codeThrows(exception)
  val eException = build
  resetBuilder

  val conc1 = e1.tree.root
  val conc2 = e2.tree.root
  val concException = eException.tree.root

  "A trace block without an engine call" should "return the result and an empty list " in {
    val (r, t) = Engine.trace { "result" }
    assertEquals(Right("result"), r)
    assertEquals(List(), t)
  }

  "A simple engine being traced" should "report the trace item" in {
    val (r, t) = Engine.trace { e1.applyParams("x") }
    assertEquals(Right(result("X")), r)
    assertEquals(List(TraceItem(e1, params("x"), Right(result("X")), Some(conc1), List(), 0)), t)
  }

  "Simple engine calls being traced " should "report the trace items" in {
    val (r, t) = Engine.trace { e1.applyParams("x"); e2.applyParams("y") }
    assertEquals(Right(result("Y")), r)
    assertEquals(List(
      TraceItem(e1, params("x"), Right(result("X")), Some(conc1), List(), 0),
      TraceItem(e2, params("y"), Right(result("Y")), Some(conc2), List(), 0)), t)
  }

  it should "report exceptions throw before conclusion found with the conclusion" in {
    val (r, t) = Engine.trace { eException.applyParams("x") }
    assertEquals(Left(exception), r)
    assertEquals(List(TraceItem(eException, params("x"), Left(exception), Some(conc1), List(), 0)), t)
  }
  //  it should "report exceptions throw before conclusion found, with None as conclusion" in {
  //    val (r, t) = Engine.trace { eException1("x") }
  //    assertEquals(ROrException(exception), r)
  //    assertEquals(List(TraceItem(eException1, List("x"), List(), eException1Conclusion, ROrException(exception), 0)), t)
  //  }
  //
  //  "An engine being traced" should "report child trace items" in {
  //    val (r, t) = Engine.trace { e("x") }
  //    assertEquals(ROrException("[x_1,x_2]"), r)
  //    assertEquals(List(TraceItem(e, List("x"), List(TraceItem(e1, List("x"), List(), e1Conclusion, ROrException("x_1"), 0), TraceItem(e2, List("x"), List(), e2Conclusion, ROrException("x_2"), 0)), eConclusion, ROrException("[x_1,x_2]"), 0)), t)
  //  }
  //
  //  it should "report nested exceptions" in {
  //    val (r, t) = Engine.trace { eException("x") }
  //    assertEquals(ROrException(exception), r)
  //    assertEquals(List(TraceItem(eException, List("x"), List(TraceItem(eException1, List("x"), List(), eException1Conclusion, ROrException(exception), 0)), eExceptionConclusion, ROrException(exception), 0)), t)
  //  }
  //
  //  "An engine being traced" should "ignore traceitems for engines in the 'ignore' list" in {
  //    val (r, t) = Engine.trace(e("x"), ignore = List(e1))
  //    assertEquals(ROrException("[x_1,x_2]"), r)
  //    assertEquals(List(TraceItem(e, List("x"), List(TraceItem(e2, List("x"), List(), e2Conclusion, ROrException("x_2"), 0)), eConclusion, ROrException("[x_1,x_2]"), 0)), t)
  //  }
  //
  //  "An engine with children being traced" should "report itself and it's children " in {
  //    val eWithC = Engine.folding[Int, String, List[String]]((acc, v) => v :: acc, List()).title("folding").
  //      childEngine("ce1").scenario(0).expected("0").code((i: Int) => i.toString).
  //      childEngine("ce2").scenario(0).expected("00").code((i: Int) => i.toString + i.toString).
  //      build
  //    val ce1 = eWithC.all(classOf[Engine])(0)
  //    val ce2 = eWithC.all(classOf[Engine])(1)
  //    val ce1Conclusion = ce1.asInstanceOf[EngineBuiltFromTests[String]].findConclusionFor(List(0))
  //    val ce2Conclusion = ce2.asInstanceOf[EngineBuiltFromTests[String]].findConclusionFor(List(0))
  //
  //    val (r, t) = Engine.trace(eWithC(0))
  //    assertEquals(ROrException(List("00", "0")), r)
  //    assertEquals(List(TraceItem(eWithC, List(0), List(
  //      TraceItem(ce1, List(0), List(), ce1Conclusion, ROrException("0"), 0),
  //      TraceItem(ce2, List(0), List(), ce2Conclusion, ROrException("00"), 0)),
  //      None, ROrException(List("00", "0")), 0)), t)
  //  }

}
abstract class Trace1Test[P, R] extends TraceTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class Trace2Test[P1, P2, R] extends TraceTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class Trace3Test[P1, P2, P3, R] extends TraceTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class TraceStringStringTest extends Trace1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class TraceStringStringStringTest extends Trace2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class TraceStringStringStringStringTest extends Trace3Test[String, String, String, String] with StringStringStringStringTest

