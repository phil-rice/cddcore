package org.cddcore.htmlRendering

import scala.language.implicitConversions
import org.cddcore.engine._
import org.cddcore.engine.builder.Decision
import org.cddcore.engine.builder.DecisionTreeNode
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ReportableHelper._
import org.cddcore.utilities.NestedHolder
import org.cddcore.utilities.Lists
import org.cddcore.utilities.StartChildEndType
import SampleContexts._
import StartChildEndType._
import java.util.Date
import org.cddcore.engine.builder.ElseClause

@RunWith(classOf[JUnitRunner])
class FocusedReportIntegrationTest extends AbstractTest {

  import SampleContexts._
  "A focusedReport for a use case" should "have report paths include the children,and decision trees added after engine children" in {
    val report = Report.focusedReport(Some("title"),  List(uc0, eWithUsecasesAndScenariosEd))
    val expected = List(
      List(report),
      List(eWithUsecasesAndScenariosEd, report),
      List(uc0, eWithUsecasesAndScenariosEd, report),
      List(uc0s0, uc0, eWithUsecasesAndScenariosEd, report),
      List(tree, eWithUsecasesAndScenariosEd, report),
      List(decision, tree, eWithUsecasesAndScenariosEd, report),
      List(conclusionYes, decision, tree, eWithUsecasesAndScenariosEd, report),
      List(ElseClause(), decision, tree, eWithUsecasesAndScenariosEd, report),
      List(conclusionNo, decision, tree, eWithUsecasesAndScenariosEd, report))
    val actual = report.reportPaths
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

  "A focusedReport for a  engine with tests" should "walk through all the children then the decision tree" in {
    val report = Report.focusedReport(Some("title"),List(eWithUsecasesAndScenariosEd))
    val expected = List(
      List(report),
      List(eWithUsecasesAndScenariosEd, report),
      List(uc0, eWithUsecasesAndScenariosEd, report),
      List(uc0s0, uc0, eWithUsecasesAndScenariosEd, report),
      List(uc1, eWithUsecasesAndScenariosEd, report),
      List(uc1s1, uc1, eWithUsecasesAndScenariosEd, report),
      List(uc1s2, uc1, eWithUsecasesAndScenariosEd, report),
      List(tree, eWithUsecasesAndScenariosEd, report),
      List(decision, tree, eWithUsecasesAndScenariosEd, report),
      List(conclusionYes, decision, tree, eWithUsecasesAndScenariosEd, report),
      List(ElseClause(), decision, tree, eWithUsecasesAndScenariosEd, report),
      List(conclusionNo, decision, tree, eWithUsecasesAndScenariosEd, report))
    val actual = report.reportPaths
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

  "A focusedReport for a FoldingEngine" should "have all the decision tress" in {
    val fed = foldingED
    val report = Report.focusedReport(Some("title"),  List(fed))
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

}
