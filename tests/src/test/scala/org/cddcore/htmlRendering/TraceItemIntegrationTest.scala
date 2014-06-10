package org.cddcore.htmlRendering

import scala.language.implicitConversions
import org.cddcore.engine.AbstractTest
import org.cddcore.engine.Engine
import org.cddcore.engine.Reportable
import org.cddcore.engine.builder.ElseClause
import org.junit.runner.RunWith
import SampleContexts.ce0ED
import SampleContexts.ce0TI
import SampleContexts.ce0Tree
import SampleContexts.ce0s0
import SampleContexts.ce1ED
import SampleContexts.ce1TI
import SampleContexts.ce1Tree
import SampleContexts.ce1s1
import SampleContexts.ce1s2
import SampleContexts.concCe0
import SampleContexts.concNoCe1
import SampleContexts.concYesCe1
import SampleContexts.decisionCe1
import SampleContexts.foldingED
import SampleContexts.foldingTI
import SampleContexts.foldingTraceReport
import SampleContexts.trace
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.ReportableHelper
import org.cddcore.engine.EngineDescription
import org.cddcore.utilities.TraceItem

@RunWith(classOf[JUnitRunner])
class TraceItemIntegrationTest extends AbstractTest {

  import SampleContexts._

  "TraceReport" should "generate report paths based on trace items" in {

    //    val expected = List(List(fullTI), List(ce0TI, fullTI), List(ce1TI, fullTI))
    val report = foldingTraceReport
    val actual = report.reportPaths
    val expected: List[List[Reportable]] = List(
      List(report),
      List(foldingTI, report),
      List(ce0TI, foldingTI, report),

      List(ce0Tree, ce0TI, foldingTI, report),
      List(concCe0, ce0Tree, ce0TI, foldingTI, report),
      List(ce1TI, foldingTI, report),
      List(ce1Tree, ce1TI, foldingTI, report),
      List(decisionCe1, ce1Tree, ce1TI, foldingTI, report),
      List(concYesCe1, decisionCe1, ce1Tree, ce1TI, foldingTI, report),
      List(ElseClause(), decisionCe1, ce1Tree, ce1TI, foldingTI, report),
      List(concNoCe1, decisionCe1, ce1Tree, ce1TI, foldingTI, report))
    for ((e, a) <- expected.zipAll(actual, null, null))
      if (e != a) {
        val compare = e.map(_.getClass.getSimpleName).zipAll(a.map(_.getClass.getSimpleName), null, null)
        assertEquals(e, a)
      }
    assertEquals(expected, actual)
    println(Report.htmlFromTrace("title", trace))
  }

  it should "generate urlMapPaths based in traceItems and the requirements of the engines referenced" in {
    val actual = foldingTraceReport.urlMapPaths
    val expected = List(
      List(foldingED),
      List(ce0ED, foldingED),
      List(ce0s0, ce0ED, foldingED),
      List(ce0Tree, ce0ED, foldingED),
      List(concCe0, ce0Tree, ce0ED, foldingED),
      List(ce1ED, foldingED),
      List(ce1s1, ce1ED, foldingED),
      List(ce1s2, ce1ED, foldingED),
      List(ce1Tree, ce1ED, foldingED),
      List(decisionCe1, ce1Tree, ce1ED, foldingED),
      List(concYesCe1, decisionCe1, ce1Tree, ce1ED, foldingED),
      List(ElseClause(), decisionCe1, ce1Tree, ce1ED, foldingED),
      List(concNoCe1, decisionCe1, ce1Tree, ce1ED, foldingED),
      List(foldingTI),
      List(ce0TI, foldingTI),
      List(ce1TI, foldingTI))
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)

  }

  it should "generate ulrMapPaths for engines that are called in  code clauses" in {
    import ReportableHelper._
    val times2 = Engine[Int, Int].title("times two").scenario(2).expected(4).code(_ * 2).build
    val plus2 = Engine[Int, Int].title("plus two").scenario(2).expected(4).code(_ + 2).build
    val times2plus2 = Engine[Int, Int].title("times two plus two").scenario(2).expected(6).code((x: Int) => plus2(times2(x))).build

    val times2ED = times2.asRequirement.asInstanceOf[EngineDescription[_, _]]
    val plus2ED = plus2.asRequirement.asInstanceOf[EngineDescription[_, _]]
    val times2plus2ED = times2plus2.asRequirement.asInstanceOf[EngineDescription[_, _]]
    val times2plus2Tree = times2plus2ED.tree.get

    val times2scenario = times2ED.scenarios(0)
    val times2tree = times2ED.tree.get
    
    val plus2scenario = plus2ED.scenarios(0)
    val plus2tree= plus2ED.tree.get
    
    val times2plus2scenario = times2plus2ED.scenarios(0)

    type TI = TraceItem[Any, Any, Any, Any]

    val times2TI: TI = TraceItem(times2, 2, Right(4), None, List(), 0)
    val plus2TI: TI = TraceItem(plus2, 4, Right(6), None, List(), 0)
    val times2plus2TI: TI = TraceItem(times2plus2, 2, Right(6), None, List(times2TI, plus2TI), 0)

    val expected = List[Any](
      List(times2plus2ED),
      List(times2plus2scenario, times2plus2ED),
      List(times2plus2Tree, times2plus2ED),
      List(times2plus2Tree.root, times2plus2Tree, times2plus2ED),
      
      List(times2ED),
      List(times2scenario, times2ED),
      List(times2tree, times2ED),
      List(times2tree.root, times2tree, times2ED),
      
      List(plus2ED),
      List(plus2scenario, plus2ED),
      List(plus2tree, plus2ED),
      List(plus2tree.root, plus2tree, plus2ED),

      List(times2plus2TI),
      List(times2TI, times2plus2TI),
      List(plus2TI, times2plus2TI))
      
    val (result, trace) = Engine.trace { times2plus2(2) }
    val report = Report.traceReport(Some("title"), trace)
    val actual = report.urlMapPaths

    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
  }


}
