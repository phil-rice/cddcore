package org.cddcore.legacy

import org.junit.runner.RunWith
import org.cddcore.engine._
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.engine.builder.Engine1FromTests
import org.cddcore.engine.builder.ElseClause
import org.cddcore.htmlRendering.Report
import org.cddcore.htmlRendering.SampleContexts
import org.cddcore.htmlRendering.RenderContext

@RunWith(classOf[JUnitRunner])
class LegacyTest extends AbstractTest {
  import LegacySamples._
  import EngineTools._

  "The legacy test" should "be set up properly" in {
    assertEquals(Some(Right("X")), conclusionX.scenarios.head.expected)
    assertEquals(Some(Right("XX")), conclusionXX.scenarios.head.expected)
    assertEquals(Some(Right("pass")), passConclusion.scenarios.head.expected)
    assertEquals(Some(Right("fail")), failConclusion.scenarios.head.expected)
  }

  "A Legacy" should "Execute ids and pass relevant parameters to the reporter" in {
    val monitor = new MemoryLegacyMonitor[Int, Int, String]
    val legacy = makeLegacy(List(1, 2, 3), monitor)

    assertEquals(List(2, 1), monitor.itemsFor(passConclusion).map(_.id))
    assertEquals(List(3), monitor.itemsFor(failConclusion).map(_.id))
    assertEquals(List(3, 1), monitor.itemsFor(conclusionX).map(_.id))
    assertEquals(List(2), monitor.itemsFor(conclusionXX).map(_.id))

    assertEquals(List(failConclusion), monitor.itemsFor(failConclusion).map(_.categoriseConclusion))
    assertEquals(List(conclusionX), monitor.itemsFor(failConclusion).map(_.replacementConclusion))
  }

  def categoriserDecisionTree(report: Report) = List(
    List(categoriserEngine.tree, categoriserEngine.asRequirement, report),
    List(categoriserRoot, categoriserEngine.tree, categoriserEngine.asRequirement, report),
    List(passConclusion, categoriserRoot, categoriserEngine.tree, categoriserEngine.asRequirement, report),
    List(ElseClause(), categoriserRoot, categoriserEngine.tree, categoriserEngine.asRequirement, report),
    List(notPassDecision, categoriserRoot, categoriserEngine.tree, categoriserEngine.asRequirement, report),
    List(failConclusion, notPassDecision, categoriserRoot, categoriserEngine.tree, categoriserEngine.asRequirement, report),
    List(ElseClause(), notPassDecision, categoriserRoot, categoriserEngine.tree, categoriserEngine.asRequirement, report),
    List(undecidedConclusion, notPassDecision, categoriserRoot, categoriserEngine.tree, categoriserEngine.asRequirement, report))

  def replacementDecisionTree(report: Report) = List(
    List(replacementEngine.tree, replacementEngine.asRequirement, report),
    List(replacementRoot, replacementEngine.tree, replacementEngine.asRequirement, report),
    List(conclusionXX, replacementRoot, replacementEngine.tree, replacementEngine.asRequirement, report),
    List(ElseClause(), replacementRoot, replacementEngine.tree, replacementEngine.asRequirement, report),
    List(conclusionX, replacementRoot, replacementEngine.tree, replacementEngine.asRequirement, report))

  "A legacy reports urlMaps" should "not throw an exception" in {
    import SampleContexts._
    import LegacySamples._
    val context: RenderContext = legacyReport

  }

  "The Legacy report, with no conclusion" should "return a path for the report, the  two engines, their conclusions some items , and the decision tree for each engine" in {
    val monitor = new MemoryLegacyMonitor[Int, Int, String]
    val legacy = makeLegacy(List(1, 2, 3), monitor)
    val report = LegacyReport("title", legacy, monitor)
    val li1 = monitor.idToItem(1)
    val li2 = monitor.idToItem(2)
    val li3 = monitor.idToItem(3)

    val expected =
      List(
        List(report),
        List(categoriserEngine.asRequirement, report),
        List(passTitle, categoriserEngine.asRequirement, report),
        List(li1, passTitle, categoriserEngine.asRequirement, report),
        List(li2, passTitle, categoriserEngine.asRequirement, report),
        List(failTitle, categoriserEngine.asRequirement, report),
        List(li3, failTitle, categoriserEngine.asRequirement, report)) :::
        categoriserDecisionTree(report) ::: List(
          List(replacementEngine.asRequirement, report),
          List(conclusionXXTitle, replacementEngine.asRequirement, report),
          List(li2, conclusionXXTitle, replacementEngine.asRequirement, report),
          List(conclusionXTitle, replacementEngine.asRequirement, report),
          List(li1, conclusionXTitle, replacementEngine.asRequirement, report),
          List(li3, conclusionXTitle, replacementEngine.asRequirement, report)) :::
          replacementDecisionTree(report)

    val actual = report.reportPaths
    assertEquals(expected, actual)
  }

  it should "cap the number of items if that is specified" in {
    val monitor = new MemoryLegacyMonitor[Int, Int, String]
    val legacy = makeLegacy(List(1, 2, 3), monitor)
    val report = LegacyReport("title", legacy, monitor, itemstoDisplay = 1)
    val li1 = monitor.idToItem(1)
    val li2 = monitor.idToItem(2)
    val li3 = monitor.idToItem(3)

    val expected = List(
      List(report),
      List(categoriserEngine.asRequirement, report),
      List(passTitle, categoriserEngine.asRequirement, report),
      List(li1, passTitle, categoriserEngine.asRequirement, report),
      List(failTitle, categoriserEngine.asRequirement, report),
      List(li3, failTitle, categoriserEngine.asRequirement, report)) :::
      categoriserDecisionTree(report) ::: List(

        List(replacementEngine.asRequirement, report),
        List(conclusionXXTitle, replacementEngine.asRequirement, report),
        List(li2, conclusionXXTitle, replacementEngine.asRequirement, report),
        List(conclusionXTitle, replacementEngine.asRequirement, report),
        List(li1, conclusionXTitle, replacementEngine.asRequirement, report)) :::
        replacementDecisionTree(report)
    val actual = report.reportPaths
    assertEquals(expected, actual)
  }

  "The Legacy report, with a conclusion specified " should "return a path for the report, the  two engines, and the first N items in each conclusion, and the decision tree for each conclusion" in {
    val monitor = new MemoryLegacyMonitor[Int, Int, String]
    val legacy = makeLegacy(List(1, 2, 3), monitor)
    val report = LegacyReport("title", legacy, monitor, conclusion = Some(passConclusion))
    val li1 = monitor.idToItem(1)
    val li2 = monitor.idToItem(2)
    val li3 = monitor.idToItem(3)

    val expected = List(
      List(report),
      List(categoriserEngine.asRequirement, report),
      List(passTitle, categoriserEngine.asRequirement, report),
      List(li1, passTitle, categoriserEngine.asRequirement, report),
      List(li2, passTitle, categoriserEngine.asRequirement, report)) :::
      categoriserDecisionTree(report) ::: List(
        List(replacementEngine.asRequirement, report),
        List(conclusionXXTitle, replacementEngine.asRequirement, report),
        List(li2, conclusionXXTitle, replacementEngine.asRequirement, report),
        List(conclusionXTitle, replacementEngine.asRequirement, report),
        List(li1, conclusionXTitle, replacementEngine.asRequirement, report)) :::
        replacementDecisionTree(report)
    val actual = report.reportPaths
    assertEquals(expected, actual)
  }

  "A legacy report" should "be rendered properly" in {
    val monitor = new MemoryLegacyMonitor[Int, Int, String]
    val legacy = makeLegacy(List(1, 2, 3), monitor)
    val report = LegacyReport("title", legacy, monitor)
    val html = Report.html(report, LegacyReportRenderer.legacyRenderer)
    println(html);

  }
} 