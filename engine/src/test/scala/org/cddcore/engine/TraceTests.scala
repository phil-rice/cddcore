package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TraceTests extends AbstractTest {
  import ConclusionOrResult._
  implicit def toEngineFromTests[R](e: Engine) = e.asInstanceOf[EngineBuiltFromTests[String]]
  val exception = new RuntimeException
  val e1 = Engine[String, String].code((s: String) => s + "_1").build;
  val e1Conclusion = e1.root.left.get
  val e2 = Engine[String, String].code((s: String) => s + "_2").build;
  val e2Conclusion = e2.root.left.get
  val e = Engine[String, String].code((s: String) => "[" + e1(s) + "," + e2(s) + "]").build;
  val eConclusion = e.root.left.get
  val eException1 = Engine[String, String].code((s: String) => throw exception).build;
  val eException1Conclusion = eException1.root.left.get
  val eException = Engine[String, String].code((s: String) => "[" + eException1(s) + "," + e2(s) + "]").build;
  val eExceptionConclusion = eException.root.left.get

  "A trace block without an engine call" should "return the result and an empty list " in {
    val (r, t) = Engine.trace { "result" }
    assertEquals(ROrException("result"), r)
    assertEquals(List(), t)
  }

  "A simple engine being traced" should "report the trace item" in {
    val (r, t) = Engine.trace { e1("x") }
    assertEquals(ROrException("x_1"), r)
    assertEquals(List(TraceItem(e1, List("x"), List(), e1Conclusion, ROrException("x_1"), 0)), t)
  }

  "Simple engine calls being traced " should "report the trace items" in {
    val (r, t) = Engine.trace { e1("x"); e2("y") }
    assertEquals(ROrException("y_2"), r)
    assertEquals(List(TraceItem(e1, List("x"), List(), e1Conclusion, ROrException("x_1"), 0), TraceItem(e2, List("y"), List(), e2Conclusion, ROrException("y_2"), 0)), t)
  }

  it should "report exceptions" in {
    val (r, t) = Engine.trace { eException1("x") }
    assertEquals(ROrException(exception), r)
    assertEquals(List(TraceItem(eException1, List("x"), List(), eException1Conclusion, ROrException(exception), 0)), t)
  }

  "An engine being traced" should "report child trace items" in {
    val (r, t) = Engine.trace { e("x") }
    assertEquals(ROrException("[x_1,x_2]"), r)
    assertEquals(List(TraceItem(e, List("x"), List(TraceItem(e1, List("x"), List(), e1Conclusion, ROrException("x_1"), 0), TraceItem(e2, List("x"), List(), e2Conclusion, ROrException("x_2"), 0)), eConclusion, ROrException("[x_1,x_2]"), 0)), t)
  }

  it should "report nested exceptions" in {
    val (r, t) = Engine.trace { eException("x") }
    assertEquals(ROrException(exception), r)
    assertEquals(List(TraceItem(eException, List("x"), List(TraceItem(eException1, List("x"), List(), eException1Conclusion, ROrException(exception), 0)), eExceptionConclusion, ROrException(exception), 0)), t)
  }

  "An engine being traced" should "ignore traceitems for engines in the 'ignore' list" in {
    val (r, t) = Engine.trace(e("x"), ignore = List(e1))
    assertEquals(ROrException("[x_1,x_2]"), r)
    assertEquals(List(TraceItem(e, List("x"), List(TraceItem(e2, List("x"), List(), e2Conclusion, ROrException("x_2"), 0)), eConclusion, ROrException("[x_1,x_2]"), 0)), t)
  }

  "An engine with children being traced" should "report itself and it's children " in {
    val eWithC = Engine.folding[Int, String, List[String]]((acc, v) => v :: acc, List()).title("folding").
      childEngine("ce1").scenario(0).expected("0").code((i: Int) => i.toString).
      childEngine("ce2").scenario(0).expected("00").code((i: Int) => i.toString + i.toString).
      build
    val ce1 = eWithC.all(classOf[Engine])(0)
    val ce2 = eWithC.all(classOf[Engine])(1)
    val ce1Conclusion = ce1.asInstanceOf[EngineBuiltFromTests[String]].findConclusionFor(List(0))
    val ce2Conclusion = ce2.asInstanceOf[EngineBuiltFromTests[String]].findConclusionFor(List(0))

    val (r, t) = Engine.trace(eWithC(0))
    assertEquals(ROrException(List("00", "0")), r)
    assertEquals(List(TraceItem(eWithC, List(0), List(
      TraceItem(ce1, List(0), List(), ce1Conclusion, ROrException("0"), 0),
      TraceItem(ce2, List(0), List(), ce2Conclusion, ROrException("00"), 0)),
      None, ROrException(List("00", "0")), 0)), t)
  }

}