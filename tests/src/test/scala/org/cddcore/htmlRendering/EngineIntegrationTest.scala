package org.cddcore.htmlRendering

import scala.language.implicitConversions


import org.cddcore.engine._
import org.cddcore.engine.builder._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.utilities._

import SampleContexts._
import StartChildEndType._
import ReportableHelper._
import EngineTools._
import ReportableHelper._

@RunWith(classOf[JUnitRunner])
class EngineIntegrationTest extends AbstractTest with SomeHoldersForTest {

  "EngineReport" should "have report paths that goes down the engineDescription and any children" in {

    val report = engineReport
    val engine = eWithUsecasesAndScenarios
    val ed = eWithUsecasesAndScenariosEd
    val expected = List(
      List(report),
      List(ed, report),
      List(uc0, ed, report),
      List(uc0s0, uc0, ed, report),
      List(uc1, ed, report),
      List(uc1s1, uc1, ed, report),
      List(uc1s2, uc1, ed, report),
      List(tree, ed, report),
      List(decision, tree, ed, report),
      List(conclusionYes, decision, tree, ed, report),
      List(ElseClause(), decision, tree, ed, report),
      List(conclusionNo, decision, tree, ed, report))
    val actual = report.reportPaths
    val t = tree.pathsIncludingSelf(List(ed, report)).toList
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

  "EngineReport's pathsToStartChildAndEnd" should "go through the report, document / engine holders and engines." in {
    val report = engineReport
    val actual = Lists.traversableToStartChildEnd(report.reportPaths)
    val engine = eWithUsecasesAndScenarios
    val ed = eWithUsecasesAndScenariosEd
    val expected = List(
      (List(report), Start),
      (List(ed, report), Start),
      (List(uc0, ed, report), Start),
      (List(uc0s0, uc0, ed, report), Child),
      (List(uc0, ed, report), End),
      (List(uc1, ed, report), Start),
      (List(uc1s1, uc1, ed, report), Child),
      (List(uc1s2, uc1, ed, report), Child),
      (List(uc1, ed, report), End),

      (List(tree, ed, report), Start),
      (List(decision, tree, ed, report), Start),
      (List(conclusionYes,decision, tree, ed, report), Child),
      (List(ElseClause(),decision, tree, ed, report), Child),
      (List(conclusionNo,decision, tree, ed, report), Child),
      (List(decision, tree, ed, report), End),
      (List(tree, ed, report), End),
      (List(ed, report), End),
      (List(report), End))

    assertEquals(expected, actual)
  }
  
  

}
