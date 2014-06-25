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

object ReportDetails {
  def apply() = new SimpleReportDetails
}
trait ReportDetails {
  def css: String
  def reportStart(title: String, iconUrl: String, date: Date): String
  def reportEnd: String
  def reportDateFormatter: DateFormat
}

class SimpleReportDetails(
  val css: String = Files.getFromClassPath(classOf[ReportDetails], "cdd.css"),
  val reportStartTemplate: String = Files.getFromClassPath(classOf[ReportDetails], "reportStart"),
  val reportEnd: String = Files.getFromClassPath(classOf[ReportDetails], "reportEnd"),
  val reportDateFormatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm")) extends ReportDetails {
  def reportStart(title: String, iconUrl: String, date: Date) =
    reportStartTemplate.
      replace("$REPORT_TITLE$", title).
      replace("$REPORT_DATE$", reportDateFormatter.format(date)).
      replace("$CSS$", css).
      replace("$ICON_URL$", iconUrl)

}

case class RenderContext(urlMap: UrlMap, reportDate: Date, iconUrl: String, pathToConclusion: List[Reportable] = List(), reportDetails: ReportDetails = ReportDetails(), misc: Map[String,Any] = Map())(implicit cddDisplayProcessor: CddDisplayProcessor) {
  val cdp = cddDisplayProcessor
  override def toString = getClass.getSimpleName()
}

object HtmlRenderer extends DecisionTreeBuilderForTests2[RenderContext, StartChildEndType, Elem] {
  import SampleContexts._
  import BuilderPimper._
  def indent(path: List[Reportable]) = path.indexWhere(_.isInstanceOf[DecisionTree[_, _]]) match {
    case 1 => ""
    case i => s"<div class='indent'>${List.fill(i - 1)("&#160;").mkString("")}</div>"
  }

  type PathAndTag = (List[Reportable], StartChildEndType)

  import TemplateLike._

  val icon = Engine[RenderContext, Reportable, String]().title("icon").description("returns the html for an image for the icon for the scenario").
    code((_, _) => "<!-- no icon -->").
    useCase("Engine from tests have icon and title equal to engine titleString").
    scenario(eBlankTitleReport, eBlankTitle).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='EBlankTitle' />").
    matchOn { case (_, e: EngineFromTests[_, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='${Strings.htmlEscape(e.titleString)}' />" }.

    useCase("Engine Descriptions have icon and title equal to engine titleString").
    scenario(eBlankTitleReport, eBlankTitleED).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='EBlankTitle' />").
    matchOn { case (_, e: EngineDescription[_, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='${Strings.htmlEscape(e.titleString)}' />" }.

    useCase("Folding Engine Descriptions have icon and title equal to engine titleString").
    scenario(foldingEngineReport, foldingED).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engineFold2_zpsb62930b9.png' alt='folding engine icon' title='Folding Engine Title' />").
    matchOn { case (_, e: FoldingEngineDescription[_, _, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engineFold2_zpsb62930b9.png' alt='folding engine icon' title='${Strings.htmlEscape(e.titleString)}' />" }.

    useCase("TraceItems have no icon or title").
    scenario(foldingEngineReport, actualFoldingTI).expected("<!-- no icon -->").
    matchOn { case (_, ti: TI) => "<!-- no icon -->" }.

    useCase("Usescase  have icon and title equal to titleString").
    scenario(engineReport, uc0).expected("<img	src='http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png' alt='usecase icon' title='useCase0'/>").
    matchOn { case (_, u: UseCase[_, _]) => s"<img	src='http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png' alt='usecase icon' title='${Strings.htmlEscape(u.titleString)}'/>" }.

    useCase("Scenarios  have icon and title equal to parameters").
    scenario(engineReport, uc0s0).expected("<img height='15' width='15' src='http://www.constraintdrivendevelopment.org/mediawiki/images/7/73/Scenario.png' alt='scenario icon' title='0'/>").
    matchOn {
      case (rc, s: AnyScenario) =>
        s"<img height='15' width='15' src='http://www.constraintdrivendevelopment.org/mediawiki/images/7/73/Scenario.png' alt='scenario icon' title='${Strings.htmlTooltipEscape(rc.cdp(s.toParams))}'/>"
    }.
    build

  //  println("Icon:\n" + icon)

  val linkAndIcon = Engine[RenderContext, Reportable, String]().title("linkAndIcon").description("Displays a suitable icon in a link for the reportable").
    code { case (rc, r) => s"<a href='${rc.urlMap(r)}'>${icon(rc, r)}</a>" }.
    scenario(context(reqWithTitleReport), reqWithTitle).
    expected(s"<a href='RootUrl/ReportTitle/ReqTitle.RequirementForTest.html'>${icon(reqWithTitleReport, reqWithTitle)}</a>").
    build

  val titleAndIcon = Engine[RenderContext, Reportable, String]().title("titleAndIcon").description("Finds a suitable titleAndIcon for a reportable. Includes links to go to item, and the id from the urlmap").
    useCase("Items that are requirements with titles use their titles").
    scenario(context(reqWithTitleReport), reqWithTitle).
    expected(s"<a id='RequirementForTest_${reqWithTitle.textOrder}' href='RootUrl/ReportTitle/ReqTitle.RequirementForTest.html'>ReqTitle<!-- no icon --></a>").
    matchOnPrim({
      case (rc, r: Requirement) if r.title.isDefined =>
        s"<a id='${UrlMap.urlId(r)}'" +
          s" href='${rc.urlMap(r)}'>${Strings.htmlEscape(r.titleString)}${icon(rc, r)}</a>"
    }, "is requirement and title is defined", "icon and title").

    useCase("Items that are reportables without titles are given template name and text order").
    scenario(context(doc1NoTitlereport), docNoTitle).
    expected { val d = s"Document_${docNoTitle.textOrder}"; s"<a id='$d' href='RootUrl/doc1Report/Document${docNoTitle.textOrder}.Document.html'>$d${icon(doc1NoTitlereport, docNoTitle)}</a>" }.
    matchOnPrim({ case (rc, r: Reportable) => s"<a id='${UrlMap.urlId(r)}' href='${rc.urlMap(r)}'>${UrlMap.urlId(r)}${icon(rc, r)}</a>" }, "reportable", "just icon").

    //    useCase("Engines are displayed based on their requirements. Without a name uses template name and text order").
    //    scenario(eBlankTitleReport, eBlankTitleED).
    //    expected { s"<a id='EngineDescription_${eBlankTitleED.textOrder}' href='RootUrl/engineReportTitle/EBlankTitle.EngineDescription.html'>EBlankTitle${icon(eBlankTitleED)}</a>" }.
    //    matchOnPrim({ case (rc, ed: EngineDescription[_, _, _, _]) => s"<a id='${UrlMap.urlId(ed)}' href='${rc.urlMap(ed)}'>${Strings.htmlEscape(ed.titleString)}${icon(ed)}</a>" }, "is engine description", "some ed stuff").

    build

  private def findTraceItemConclusion(path: List[Reportable]): List[(AnyTraceItem, AnyConclusion)] = {
    val result = path.flatMap {
      case ti: AnyTraceItem => {
        val r = ti.toEvidence[AnyConclusion].collect { case c: AnyConclusion => List((ti, c)) }.getOrElse(Nil)
        r
      }
      case _ => List()
    }
    result
  }

  val conclusionPath = Engine[RenderContext, List[Reportable], List[Reportable]].title("conclusionPath").
    useCase("most things don't have a conclusion path").
    scenario(context(foldingEngineReport, List(concCe0)), List(foldingED)).expected(List()).
    scenario(context(foldingEngineReport, List(concCe0)), List(ce0ED, foldingED)).expected(List()).
    scenario(context(foldingEngineReport, List(concCe0)), List(ce0s0, ce0ED, foldingED)).expected(List()).
    scenario(context(foldingEngineReport, List(concCe0)), List(ce0Tree, foldingED)).expected(List()).
    scenario(context(foldingEngineReport, List(concCe0)), List(ce0Tree, foldingED)).expected(List()).

    useCase("Decision Tree Nodes get a path from the RenderContext if it exists").
    scenario(context(foldingEngineReport, List(concCe0)), List(concCe0, ce0Tree, foldingED)).expected(List(concCe0)).
    matchOn { case (rc, (head: AnyDecisionTreeNode) :: tail) => rc.pathToConclusion }.
    scenario(context(foldingEngineReport, List(concCe0)), List(concCe0, ce0Tree, foldingED)).expected(List(concCe0)).
    scenario(context(foldingEngineReport, List(concCe0)), List(concCe0, ce0Tree, foldingED)).expected(List(concCe0)).
    scenario(context(foldingEngineReport, List(concYesCe1, decisionCe1)), List(decisionCe1, ce0Tree, foldingED)).expected(List(concYesCe1, decisionCe1)).
    scenario(context(foldingEngineReport, List(concYesCe1, decisionCe1)), List(concYesCe1, decisionCe1, ce0Tree, foldingED)).expected(List(concYesCe1, decisionCe1)).

    useCase("Decision Tree Nodes get path from TraceItem if it exists").
    scenario(context(foldingEngineReport, List()), List(concCe0, ce0Tree, ce0ED, actualCe0TI, actualFoldingTI)).expected(List(concCe0)).
    matchOn {
      case (rc, path) if !findTraceItemConclusion(path).isEmpty => {
        import EngineTools._
        val cPath = findTraceItemConclusion(path)
        cPath match {
          case (ti: AnyTraceItem, c) :: _ =>
            val e = ti.toMain[EngineFromTests[_, _]]
            e.evaluator.findPathToConclusionWithConclusion(e.tree.root, c, List())
          case _ =>
            List()
        }
      }
    }.
    scenario(context(foldingEngineReport, List()), List(decisionCe1, ce1Tree, ce1ED, actualCe1TI, actualFoldingTI)).expected(List(concYesCe1, decisionCe1)).
    scenario(context(foldingEngineReport, List()), List(concYesCe1, decisionCe1, ce1Tree, ce1ED, actualCe1TI, actualFoldingTI)).expected(List(concYesCe1, decisionCe1)).
    scenario(context(foldingEngineReport, List()), List(concNoCe1, decisionCe1, ce1Tree, ce1ED, actualCe1TI, actualFoldingTI)).expected(List(concYesCe1, decisionCe1)).
    build

  val engineAndDocumentsSingleItemRenderer = Engine[RenderContext, List[Reportable], StartChildEndType, String]().
    title("Engine and Documents Single Item Renderer").
    renderReport.
    renderEngineHolders.
    renderEnginesAsLineItem.
    renderFoldingEnginesAsSmallTree.
    renderDocumentHolders.
    renderDocuments.
    build

  val engineReportSingleItemRenderer = Engine[RenderContext, List[Reportable], StartChildEndType, String]().title("Single Engine report").
    renderReport.
    renderFoldingEngines.
    renderChildEngines.
    renderEngineFromTests.
    renderUseCasesWhenScenariosAreSummaries.
    renderScenarioAsIcons.
    renderDecisionTrees.
    build

  val traceReportSingleItemRenderer = Engine[RenderContext, List[Reportable], StartChildEndType, String]().title("Trace Item Report").
    renderReport.
    renderTraceItems.
    renderFoldingEngines.
    renderChildEngines.
    renderEngineFromTests.
    renderUseCasesWhenScenariosAreSummaries.
    renderScenarioAsIcons.
    renderDecisionTreesForTraceItems.
    build

  val useCaseOrScenarioReportRenderer = Engine[RenderContext, List[Reportable], StartChildEndType, String]().title("Single Use Case Report").
    renderReport.
    renderFoldingEngines.
    renderChildEngines.
    renderEngineFromTests.
    renderUseCasesWhenScenariosAreDetailed.
    renderScenarioDetailed.
    renderDecisionTrees.
    build

  val rendererFor =
    Engine[Reportable, Engine3[RenderContext, List[Reportable], StartChildEndType, String, String]]().title("Select the renderer for use for this reportable").
      useCase("Engines produce engineReportSingleItemRenderer").
      scenario(foldingED).expected(engineReportSingleItemRenderer).
      matchOn { case _: EngineRequirement[_, _] => engineReportSingleItemRenderer }.
      scenario(ce0ED).expected(engineReportSingleItemRenderer).
      scenario(eBlankED).expected(engineReportSingleItemRenderer).

      useCase("Other requirements produce useCaseOrScenarioReportRenderer").
      scenario(uc0).expected(useCaseOrScenarioReportRenderer).
      matchOn { case _: Requirement => useCaseOrScenarioReportRenderer }.
      scenario(uc0s0).expected(useCaseOrScenarioReportRenderer).
      build

}
