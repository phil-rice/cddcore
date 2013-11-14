package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TraceTests extends AbstractTest {
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

}