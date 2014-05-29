package org.cddcore.htmlRendering

import scala.language.implicitConversions


import org.cddcore.engine._
import org.cddcore.engine.builder._
import org.cddcore.utilities._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ReportableHelper._
import SampleContexts._
import StartChildEndType._
import EngineTools._
import ReportableHelper._

@RunWith(classOf[JUnitRunner])
class FoldingEngineIntegrationTest extends AbstractTest with SomeHoldersForTest {

  "FoldingEngineReport" should "have report paths that goes down the engineDescription and any children" in {
    val report = foldingEngineReport
    val fed = foldingED
    val expected = List(
      List(report),
      List(fed, report),
      List(ce0ED, fed, report),
      List(ce0s0, ce0ED, fed, report),
      List(ce0Tree, ce0ED, fed, report),
      List(concCe0, ce0Tree, ce0ED, fed, report),
      List(ce1ED, fed, report),
      List(ce1s1, ce1ED, fed, report),
      List(ce1s2, ce1ED, fed, report),
      List(ce1Tree, ce1ED, fed, report),
      List(decisionCe1, ce1Tree, ce1ED, fed, report),
      List(concYesCe1, decisionCe1, ce1Tree, ce1ED, fed, report),
      List(ElseClause(), decisionCe1, ce1Tree, ce1ED, fed, report),
      List(concNoCe1, decisionCe1, ce1Tree, ce1ED, fed, report))
    val actual = report.reportPaths
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

  "FoldingEngineReport pathsToStartChildAndEnd" should "go through the report, document / engine holders and engines." in {
    val report = foldingEngineReport
    val fed = foldingED
    val expected = List(
      (List(report), Start),
      (List(fed, report), Start),
      (List(ce0ED, fed, report), Start),
      (List(ce0s0, ce0ED, fed, report), Child),
      (List(ce0Tree, ce0ED, fed, report), Start),
      (List(concCe0, ce0Tree, ce0ED, fed, report), Child),
      (List(ce0Tree, ce0ED, fed, report), End),
      (List(ce0ED, fed, report), End),

      (List(ce1ED, fed, report), Start),
      (List(ce1s1, ce1ED, fed, report), Child),
      (List(ce1s2, ce1ED, fed, report), Child),
      (List(ce1Tree, ce1ED, fed, report), Start),
      (List(decisionCe1, ce1Tree, ce1ED, fed, report), Start),
      (List(concYesCe1, decisionCe1, ce1Tree, ce1ED, fed, report), Child),
      (List(ElseClause(), decisionCe1, ce1Tree, ce1ED, fed, report), Child),
      (List(concNoCe1, decisionCe1, ce1Tree, ce1ED, fed, report), Child),
      (List(decisionCe1, ce1Tree, ce1ED, fed, report), End),
      (List(ce1Tree, ce1ED, fed, report), End),
      (List(ce1ED, fed, report), End),
      (List(fed, report), End),
      (List(report), End))

    val actual = Lists.traversableToStartChildEnd(report.reportPaths)
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

}
