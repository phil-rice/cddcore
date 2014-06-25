package org.cddcore.htmlRendering

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem
import org.cddcore.engine._
import org.cddcore.engine.builder._
import org.cddcore.utilities._
import org.cddcore.utilities.StartChildEndType._
import java.io.File
import scala.language.implicitConversions

object BuilderPimper {
  implicit def toPimper(builder: Builder3[RenderContext, List[Reportable], StartChildEndType, String, String]): BuilderPimper = new BuilderPimper(builder: Builder3[RenderContext, List[Reportable], StartChildEndType, String, String])
  def table(clazz: String, rows: Tuple2[String, _]*) =
    s"<table class='$clazz'>${
      rows.map {
        case (c, (n, v)) => s"<tr class='$c'><td class='title'>$n</td><td class='value'>$v</td></tr>"
        case (n, v) => s"<tr><td class='title'>$n</td><td class='value'>$v</td></tr>"
      }.mkString("")
    }</table>"
}

class BuilderPimper(builder: Builder3[RenderContext, List[Reportable], StartChildEndType, String, String]) {
  import HtmlRenderer._
  import SampleContexts._
  import BuilderPimper._
  def scenario(report: Report, item: Reportable, sce: StartChildEndType, pathToConclusion: List[Reportable] = List()): Builder3[RenderContext, List[Reportable], StartChildEndType, String, String] = {
    val reportPaths = report.reportPaths
    val path = reportPaths.find(_.head == item) match {
      case Some(p) => p
      case _ =>
        throw new IllegalArgumentException(s"\nReport: $report\nLast: $item\n${reportPaths.mkString("\n")}")
    }
    val rc = context(report).copy(pathToConclusion = pathToConclusion)
    builder.scenario(rc, path, sce)
  }
  def renderReport = builder.useCase("Reports have a huge template at the start, and end. The report title and date are substituted in").
    scenario(engineReport, engineReport, Start).
    expected(ReportDetails().reportStart("engineReportTitle", iconUrl, testDate)).
    matchOn { case (RenderContext(_, date, iconUrl, pathToConclusion, reportDetails, _), (r: Report) :: _, Start) => reportDetails.reportStart(r.titleString, iconUrl, date) }.

    scenario(engineReport, engineReport, End).
    expected(ReportDetails().reportEnd).
    matchOn { case (rc: RenderContext, (r: Report) :: _, End) => rc.reportDetails.reportEnd }

  def renderDocumentHolders = builder.useCase("Document Holders have a div, and hold the items as an unorder list").
    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_documentHolder, Start).
    expected("\n<div class='documentHolder'><h3>Documents</h3><ul>\n").
    because { case (_, (holder: DocumentHolder) :: _, Start) => true; case _ => false }.

    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_documentHolder, End).
    expected("\n</ul></div> <!-- documentHolder -->\n").
    because { case (_, (holder: DocumentHolder) :: _, End) => true; case _ => false }.

    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_documentHolder, Child).
    expected("\n<div class='documentHolder'><h3>Documents</h3><ul><li>No documents</li></ul></div> <!-- documentHolder -->\n").
    because { case (_, (holder: DocumentHolder) :: _, Child) => true; case _ => false }

  def renderDocuments = builder.useCase("Documents are list items in an anchor, and use the TitleAndIcon engine").
    scenario(eBlankTitleDoc1_DocAndEngineReport, doc1, Child).
    expected(s"\n<li>${titleAndIcon(context(eBlankTitleDoc1_DocAndEngineReport), doc1)}</li>\n").
    matchOn { case (rc, ((doc: Document) :: _), Child) => s"\n<li>${titleAndIcon(rc, doc)}</li>\n" }

  def renderEngineHolders = builder.useCase("Engine Holders have a div, and hold the items as an unorder list").
    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_DocAndEngineReport.engineHolder, Start).
    expected("\n<div class='engineHolder'><h3>Engines</h3><ul>\n").
    because { case (_, (holder: EngineHolder) :: _, Start) => true; case _ => false }.

    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_DocAndEngineReport.engineHolder, Child).
    expected("\n<div class='engineHolder'><h3>Engines</h3>No Engines</div> <!-- engineHolder -->\n").
    because { case (_, (holder: EngineHolder) :: _, Child) => true; case _ => false }.

    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_engineHolder, End).
    expected("\n</ul></div> <!-- engineHolder -->\n").
    because { case (_, (holder: EngineHolder) :: _, End) => true; case _ => false }

  def renderEnginesAsLineItem = builder.useCase("Engines are list items in an anchor, and use the titleAndIcon engine").
    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1ED, Child).
    expected(s"\n<li>${titleAndIcon(context(eBlankTitleDoc1_DocAndEngineReport), eBlankTitleDoc1ED)}</li>\n").
    matchOn { case (rc, (ed: EngineDescription[_, _]) :: _, Child) => s"\n<li>${titleAndIcon(rc, ed)}</li>\n" }

  def renderFoldingEnginesAsSmallTree = builder.useCase("Folding engines are list items, but have an unordered list of child engines").
    scenario(foldingEngineAndDocumentReport, foldingED, Start).
    expected(s"\n<li>${titleAndIcon(context(foldingEngineAndDocumentReport), foldingED)}</li><ul>\n").
    matchOn {
      case (rc, (fed: FoldingEngineDescription[_, _, _]) :: _, Start) =>
        s"\n<li>${titleAndIcon(rc, fed)}</li><ul>\n"
    }.
    scenario(foldingEngineAndDocumentReport, foldingED, End).
    expected(s"</ul>").
    matchOn {
      case (rc, (fed: FoldingEngineDescription[_, _, _]) :: _, End) =>
        s"</ul>"
    }

  def renderFoldingEngines = builder.useCase("A folding engine is rendered as a 'engineWithChildren' div. The engineWithChildrenSummary is totally redundant. Inside that is an engineText which holds the engine text, followed by the children ").
    scenario(foldingEngineReport, foldingED, Start).
    expected(s"\n<div class='engineWithChildren'><div class='engineWithChildrenSummary'><div class='engineText'>${titleAndIcon(foldingEngineReport, foldingED)}</div> <!-- engineText -->\n").
    matchOn { case (rc, (engine: FoldingEngineDescription[_, _, _]) :: _, Start) => s"\n<div class='engineWithChildren'><div class='engineWithChildrenSummary'><div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText -->\n" }.

    scenario(foldingEngineReport, foldingED, End).
    expected("\n</div> <!--engineWithChildrenSummary --></div> <!-- engineWithChildren -->\n").
    because { case (_, (engine: FoldingEngineDescription[_, _, _]) :: _, End) => true; case _ => false }

  def renderChildEngines = builder.useCase("A child engine is rendered as childEngine div within that is a engineSummary div. The children are after the engineSummary div").
    scenario(foldingEngineReport, ce0ED, Start).
    expected(s"<div class='childEngine'><div class='engineSummary'>\n<div class='engineText'>${titleAndIcon(foldingEngineReport, ce0ED)}</div> <!-- engineText -->\n").
    matchOn {
      case (rc, (engine: EngineDescription[_, _]) :: (_: FoldingEngineDescription[_, _, _]) :: _, Start) =>
        s"<div class='childEngine'><div class='engineSummary'>\n<div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText -->\n"
    }.

    scenario(foldingEngineReport, ce0ED, End).
    expected("\n</div> <!-- childEngine -->"). //the decision tree closes off the engineSummary div
    because { case (_, (engine: EngineDescription[_, _]) :: (_: FoldingEngineDescription[_, _, _]) :: _, End) => true; case _ => false }

  def renderEngineFromTests = builder.useCase("An engine from tests has a engineWithTests div, a engineSummary. The children are after the engineSummary div ").
    scenario(engineReport, eWithUsecasesAndScenariosEd, Start).
    expected(s"<div class='engineWithTests'><div class='engineSummary'>\n<div class='engineText'>${titleAndIcon(engineReport, eWithUsecasesAndScenariosEd)}</div> <!-- engineText -->\n").
    matchOn {
      case (rc, (engine: EngineDescription[_, _]) :: _, Start) =>
        s"<div class='engineWithTests'><div class='engineSummary'>\n<div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText -->\n"
    }.

    scenario(engineReport, eWithUsecasesAndScenariosEd, End).
    expected("\n</div> <!-- engineWithTests-->"). //the decision tree closes off the engineSummary div
    because { case (_, (engine: EngineDescription[_, _]) :: _, End) => true; case _ => false }

  def renderUseCasesChildren = builder.scenario(engineReport, uc0, Child).
    expected(s"\n<div class='usecaseSummary'><h4>${titleAndIcon(engineReport, uc0)}</h4>useCase0Description</div> <!-- 'usecaseSummary -->\n").
    matchOn {
      case (rc, (uc: UseCase[_, _]) :: _, Child) => s"\n<div class='usecaseSummary'>" +
        s"<h4>${titleAndIcon(rc, uc)}</h4>${uc.description.getOrElse("")}</div> <!-- 'usecaseSummary -->\n"
    }

  def renderUseCasesWhenScenariosAreSummaries = builder.useCase("a use case is a header with the title and icon in it, The scenarios are part of the header, and then the description").
    scenario(engineReport, uc0, Start).
    expected(s"\n<div class='usecaseSummary'><h4>${titleAndIcon(engineReport, uc0)}").
    matchOn { case (rc, (uc: UseCase[_, _]) :: _, Start) => s"\n<div class='usecaseSummary'><h4>${titleAndIcon(rc, uc)}" }.

    scenario(engineReport, uc0, End).
    expected("\n</h4>useCase0Description</div> <!-- usecaseSummary -->\n").
    matchOn { case (rc, (uc: UseCase[_, _]) :: _, End) => s"\n</h4>${uc.description.getOrElse("")}</div> <!-- usecaseSummary -->\n" }.

    renderUseCasesChildren

  def renderUseCasesWhenScenariosAreDetailed = builder.useCase("a use case is a header with the title and icon in it, The scenarios are part of the header, and then the description").
    scenario(engineReport, uc0, Start).
    expected(s"\n<div class='usecaseSummary'><h4>${titleAndIcon(engineReport, uc0)}</h4>useCase0Description").
    matchOn { case (rc, (uc: UseCase[_, _]) :: _, Start) => s"\n<div class='usecaseSummary'><h4>${titleAndIcon(rc, uc)}</h4>${uc.description.getOrElse("")}" }.

    scenario(engineReport, uc0, End).
    expected("\n</div> <!-- usecaseSummary -->\n").
    matchOn { case (rc, (uc: UseCase[_, _]) :: _, End) => s"\n</div> <!-- usecaseSummary -->\n" }.

    renderUseCasesChildren

  def renderScenarioAsIcons = builder.useCase("a scenario is just the icon").
    scenario(engineReport, uc0s0, Child).
    expected(linkAndIcon(context(engineReport), uc0s0)).
    matchOn { case (rc, (s: Scenario[_, _]) :: _, Child) => linkAndIcon(rc, s) }

  def renderScenarioDetailed = builder.useCase("a scenario is a table").
    scenario(engineReport, uc0s0, Child).
    expected(s"<div class='scenario'><div class='scenarioText'>${titleAndIcon(context(engineReport), uc0s0)}</div><!-- scenarioText -->" +
      table("scenarioTable", "Parameter" -> "0", "Expected" -> "0") + "</div><!-- scenario -->\n").
    matchOn {
      case (rc, (s: Scenario[_, _]) :: _, Child) => s"<div class='scenario'><div class='scenarioText'>${titleAndIcon(rc, s)}</div><!-- scenarioText -->" +
        table("scenarioTable", "Parameter" -> rc.cdp.html(s.params), "Expected" -> s.htmlPrintExpected) +
        "</div><!-- scenario -->\n"
    }

  def renderDecisionTrees = builder.useCase("A decision tree, closes off the engine with summary div and  get's it's own div").
    scenario(engineReport, tree, Start).
    expected("\n</div> <!-- engineSummary--><div class='decisionTree'>\n").
    because { case (_, (s: DecisionTree[_, _]) :: _, Start) => true; case _ => false }.

    scenario(engineReport, tree, End).
    expected("\n</div><!-- decisionTree -->\n").
    matchOn { case (_, (s: DecisionTree[_, _]) :: _, End) => "\n</div><!-- decisionTree -->\n" }.
    renderDecisions

  def renderDecisionTreesForTraceItems = builder.useCase("A decision tree, closes off the engine with summary div and  get's it's own div").
    scenario(engineReport, tree, Start).
    expected("\n<div class='decisionTreeForTraceItem'>\n").
    because { case (_, (s: DecisionTree[_, _]) :: _, Start) => true; case _ => false }.

    scenario(engineReport, tree, End).
    expected("\n</div><!-- decisionTreeForTraceItem -->\n").
    matchOn { case (_, (s: DecisionTree[_, _]) :: _, End) => "\n</div><!-- decisionTreeForTraceItem -->\n" }.
    renderDecisions

  private def decisionPrefix(path: List[Reportable], d: AnyDecision, clazz: String) =
    s"\n<div class='decision'><div class='$clazz'>${indent(path)}<span class='keyword'>if&#160;</span>" +
      s"\n<div class='because'>${Strings.htmlEscape(d.toPrettyString)}</div><!-- because --></div><!-- $clazz -->\n"

  private def decisionIsTrue(path: List[Reportable], d: AnyDecision) = {
    path.indexOf(d) match {
      case -1 =>
        throw new IllegalStateException
      case 0 =>
        throw new IllegalStateException
      case i => {
        val child = path(i - 1)
        if (d.toYes == child)
          true
        else if (d.toNo == child)
          false
        else
          throw new IllegalStateException
      }
    }
  }
  def conclusionPathContainsAndDecisionIs(rc: RenderContext, path: List[Reportable], d: AnyDecision, expected: Boolean) = {
    val cPath = conclusionPath(rc, path)
    if (cPath.contains(d))
      expected == decisionIsTrue(cPath, d)
    else
      false
  }

  protected def renderDecisions = builder.
    useCase("A decision get's it's own div, and a because").
    scenario(engineReport, decision, Start).
    expected("\n<div class='decision'><div class='because'><span class='keyword'>if&#160;</span>" +
      "\n<div class='because'>x.&gt;(0)</div><!-- because --></div><!-- because -->\n").
    matchOn { case (_, path @ (d: AnyDecision) :: _, Start) => decisionPrefix(path, d, "because") }.

    useCase("A decision that is in the pathToConclusion needs to be marked 'true' or 'false'").
    scenario(engineReport, decision, Start, List(conclusionYes, decision)).
    expected("\n<div class='decision'><div class='ifTrueOnPath'><span class='keyword'>if&#160;</span>" +
      "\n<div class='because'>x.&gt;(0)</div><!-- because --></div><!-- ifTrueOnPath -->\n").
    matchOn { case (rc, path @ (d: AnyDecision) :: _, Start) if conclusionPathContainsAndDecisionIs(rc, path, d, true) => decisionPrefix(path, d, "ifTrueOnPath") }.

    scenario(engineReport, decision, Start, List(conclusionNo, decision)).
    expected("\n<div class='decision'><div class='ifFalseOnPath'><span class='keyword'>if&#160;</span>" +
      "\n<div class='because'>x.&gt;(0)</div><!-- because --></div><!-- ifFalseOnPath -->\n").
    matchOn {
      case (rc, path @ (d: AnyDecision) :: _, Start) if conclusionPathContainsAndDecisionIs(rc, path, d, false) =>
        decisionPrefix(path, d, "ifFalseOnPath")
    }.

    scenario(engineReport, decision, End).
    expected("</div><!--decision -->\n").
    matchOn { case (_, (s: AnyDecision) :: _, End) => "</div><!--decision -->\n" }.

    scenario(engineReport, decision, End, List(conclusionYes, decision)).
    expected("</div><!--decision -->\n").
    matchOn { case (rc, path @ (d: AnyDecision) :: _, End) if conclusionPath(rc, path).contains(d) => "</div><!--decision -->\n" }.

    useCase("An elseclause is artificially inserted to allow else to be displayed easily").
    scenario(engineReport, ElseClause(), Child).
    expected("<div class='else'><div class='indent'>&#160;</div><span class='keyword'>else&#160;</span></div>").
    matchOn { case (_, path @ (s: ElseClause) :: _, _) => s"<div class='else'>${indent(path)}<span class='keyword'>else&#160;</span></div>" }.

    useCase("A conclusion get's it's own div").
    scenario(engineReport, conclusionYes, Child).
    expected(s"\n<div class='result'><div class='indent'>&#160;</div><span class='keyword'>then&#160;</span>" +
      s"<a class='scenarioLink' href='RootUrl/engineReportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s1.textOrder}.Scenario.html'>${icon(engineReport, uc1s1)}</a>" +
      s"<a class='scenarioLink' href='RootUrl/engineReportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s2.textOrder}.Scenario.html'>${icon(engineReport, uc1s2)}</a>" +
      s"<div class='conclusion'>((x: Int) =&gt; x.*(2))</div><!-- conclusion --></div><!-- result -->\n").
    matchOn {
      case (rc, path @ (c: AnyConclusion) :: _, Child) => "\n" +
        s"<div class='result'>${indent(path)}<span class='keyword'>then&#160;</span>" +
        s"${
          c.toScenarios.map {
            case s if rc.urlMap.get(s).isDefined => s"<a class='scenarioLink' href='${rc.urlMap(s)}'>${icon(rc, s)}</a>"
            case s => s"<a class='scenarioLink'>${icon(rc, s)}</a>"
          }.mkString("")
        }" +
        s"<div class='conclusion'>${Strings.htmlEscape(c.toCode.description)}</div><!-- conclusion --></div><!-- result -->\n"
    }.
    useCase("A conclusion that is on the pathToConclusion needs to be marked get's it's own div").
    scenario(engineReport, conclusionYes, Child, List(conclusionYes, decision)).
    expected(s"\n<div class='resultWithTest '><div class='indent'>&#160;</div><span class='keyword'>then&#160;</span>" +
      s"<a class='scenarioLink' href='RootUrl/engineReportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s1.textOrder}.Scenario.html'>${icon(engineReport, uc1s1)}</a>" +
      s"<a class='scenarioLink' href='RootUrl/engineReportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s2.textOrder}.Scenario.html'>${icon(engineReport, uc1s2)}</a>" +
      s"<div class='conclusion'>((x: Int) =&gt; x.*(2))</div><!-- conclusion --></div><!-- result -->\n").
    matchOn {
      case (rc, path @ (c: AnyConclusion) :: _, Child) if conclusionPath(rc, path).contains(c) => "\n" +
        s"<div class='resultWithTest '>${indent(path)}<span class='keyword'>then&#160;</span>" +
        s"${c.toScenarios.map((s) => s"<a class='scenarioLink' href='${rc.urlMap(s)}'>${icon(rc, s)}</a>").mkString("")}" +
        s"<div class='conclusion'>${Strings.htmlEscape(c.toCode.description)}</div><!-- conclusion --></div><!-- result -->\n"
    }

  import EngineTools._

  def renderTraceItems = builder.useCase("A trace item ").
    scenario(foldingTraceReport, actualFoldingTI, Start).
    expected(s"\n<div class='traceItem'><div class='traceItemEngine'><table class='traceItemTable'><tr><td class='engineTitle'>Engine</td><td class='engineValue'>${titleAndIcon(foldingTraceReport, foldingED)}</td></tr><tr><td class='title'>Parameter</td><td class='value'>1</td></tr><tr><td class='title'>Result</td><td class='value'>Right(List(0, 2))</td></tr>" +
      "\n</table>\n").
    matchOn {
      case (rc, (ti @ TraceItem(engine: Engine, params, result, _, _, _, _)) :: _, Start) =>
        s"\n<div class='traceItem'><div class='traceItemEngine'>" +
          s"<table class='traceItemTable'>" +
          s"<tr><td class='engineTitle'>Engine</td><td class='engineValue'>${titleAndIcon(rc, EngineTools.toEngineTools(engine).asRequirement)}</td></tr>" +
          s"<tr><td class='title'>Parameter</td><td class='value'>${rc.cdp(params)}</td></tr>" +
          s"<tr><td class='title'>Result</td><td class='value'>${rc.cdp(result)}</td></tr>" +
          "\n</table>\n"
    }.

    scenario(foldingTraceReport, actualFoldingTI, End).
    expected("\n</div><!-- traceItemEngine --></div><!--traceItem -->\n").
    matchOnPrim({ case (rc, (ti: AnyTraceItem) :: _, End) => "\n</div><!-- traceItemEngine --></div><!--traceItem -->\n" }, "is trace item / End", "close off traceItemEngine and traceItem divs")

}