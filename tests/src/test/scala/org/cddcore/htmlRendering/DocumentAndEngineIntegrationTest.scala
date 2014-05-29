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
import java.util.Date
import org.cddcore.engine.builder.ElseClause

@RunWith(classOf[JUnitRunner])
class DocumentAndEngineIntegrationTest extends AbstractTest with SomeHoldersForTest {

  "A documentAndEngineReport" should "have report paths that go down the documents then the engines" in {
    val report = eBlankTitleDoc1_DocAndEngineReport
    val docHolder = report.documentHolder
    assertEquals(List(doc1), docHolder.nodes)
    assertEquals(List(
      List(report),
      List(report.documentHolder, report),
      List(doc1, report.documentHolder, report),
      List(report.engineHolder, report),
      List(eBlankTitleDoc1ED, report.engineHolder, report)),
      report.reportPaths)
  }

  "A documentAndEngineReport's pathsToStartChildAndEnd" should "go through the report, document / engine holders and engines." in {
    val report = eBlankTitleDoc1_DocAndEngineReport
    val actual = Lists.traversableToStartChildEnd(report.reportPaths)
    val expected = List(
      (List(report), Start),
      (List(report.documentHolder, report), Start),
      (List(doc1, report.documentHolder, report), Child),
      (List(report.documentHolder, report), End),
      (List(report.engineHolder, report), Start),
      (List(eBlankTitleDoc1ED, report.engineHolder, report), Child),
      (List(report.engineHolder, report), End),
      (List(report), End))
    //    println(Lists.dumpPathsWithStartChildEnd(actual, (x: Reportable) => x.getClass.getSimpleName))
    //    for ((e, a) <- expected.zipAll(actual, null, null))
    //      assertEquals(e, a)

    assertEquals(expected, actual)
  }

  "A documentAndEngineReport' urlMapPath" should "include the engine descriptions and tree, but not engine or document holder" in {
    val report = eBlankTitleDoc1_DocAndEngineReport
    val tree = eBlankTitleDoc1ED.tree.get
    val expected = List(
      List(report),
      List(doc1, report),
      List(eBlankTitleDoc1ED, report),
      List(tree, eBlankTitleDoc1ED, report),
      List(tree.root, tree, eBlankTitleDoc1ED, report))
    val actual = report.urlMapPaths
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }
  "A documentAndEngineReport for an engine and a folding engine's' reports" should "include the engine descriptions, but not usecases/scenarios or trees" in {
    val report = Report.documentAndEngineReport(Some("title"), new Date(), List(eBlankTitleDoc1, eWithUsecasesAndScenarios, folding))
    val eBlankTree = eBlankTitleDoc1ED.tree.get

    val expected = List(
      List(report),
      List(report.documentHolder, report),
      List(doc1, report.documentHolder, report),
      List(report.engineHolder, report),
      List(eBlankTitleDoc1ED, report.engineHolder, report),
      List(eWithUsecasesAndScenariosEd, report.engineHolder, report),

      List(foldingED, report.engineHolder, report),
      List(ce0ED, foldingED, report.engineHolder, report),
      List(ce1ED, foldingED, report.engineHolder, report))

    val actual = report.reportPaths
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }
  "A documentAndEngineReport for an engine and a folding engine's' ulrMap" should "include the engine descriptions,  usecases/scenarios and trees" in {
    val report = Report.documentAndEngineReport(Some("title"), new Date(), List(eBlankTitleDoc1, eWithUsecasesAndScenarios, folding))
    val eBlankTree = eBlankTitleDoc1ED.tree.get
    val x = eBlankTitleDoc1ED.pathsIncludingTreeAndEngine(List())
    val expected = List(
      List(report),
      List(doc1, report),
      List(eBlankTitleDoc1ED,  report),
      List(eBlankTree, eBlankTitleDoc1ED,  report),
      List(eBlankTree.root, eBlankTree, eBlankTitleDoc1ED,  report),

      List(eWithUsecasesAndScenariosEd,  report),
      List(uc0, eWithUsecasesAndScenariosEd,  report),
      List(uc0s0, uc0, eWithUsecasesAndScenariosEd,  report),
      List(uc1, eWithUsecasesAndScenariosEd,  report),
      List(uc1s1, uc1, eWithUsecasesAndScenariosEd,  report),
      List(uc1s2, uc1, eWithUsecasesAndScenariosEd,  report),
      List(tree, eWithUsecasesAndScenariosEd,  report),
      List(decision, tree, eWithUsecasesAndScenariosEd,  report),
      List(conclusionYes, decision, tree, eWithUsecasesAndScenariosEd,  report),
      List(ElseClause(), decision, tree, eWithUsecasesAndScenariosEd,  report),
      List(conclusionNo, decision, tree, eWithUsecasesAndScenariosEd,  report),
 
      List(foldingED,  report),
      List(ce0ED, foldingED,  report),
      List(ce0s0, ce0ED, foldingED,  report),
      List(ce0Tree, ce0ED, foldingED,  report),
      List(concCe0, ce0Tree, ce0ED, foldingED,  report),
      List(ce1ED, foldingED,  report),
      List(ce1s1, ce1ED, foldingED,  report),
      List(ce1s2, ce1ED, foldingED,  report),
      List(ce1Tree, ce1ED, foldingED,  report),
      List(decisionCe1, ce1Tree, ce1ED, foldingED,  report),
      List(concYesCe1, decisionCe1, ce1Tree, ce1ED, foldingED,  report),
      List(ElseClause(), decisionCe1, ce1Tree, ce1ED, foldingED,  report),
      List(concNoCe1, decisionCe1, ce1Tree, ce1ED, foldingED,  report))

    val actual = report.urlMapPaths 
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

}
