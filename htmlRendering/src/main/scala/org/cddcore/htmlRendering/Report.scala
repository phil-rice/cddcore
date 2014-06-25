package org.cddcore.htmlRendering

import java.util.Date
import org.cddcore.engine._
import org.cddcore.utilities._
import StartChildEndType._
import EngineTools._
import java.io.File
import org.cddcore.engine.builder.Conclusion
import org.cddcore.engine.builder.AnyConclusion

object Report {
  def apply(title: Option[String] = None, description: Option[String] = None, nodes: List[Reportable] = List()) =
    new SimpleReport(title, description, nodes)
  def documentAndEngineReport(title: Option[String], date: Date, engines: Traversable[Engine], description: Option[String] = None) =
    new DocumentAndEngineReport(title, engines, description)
  def engineReport(title: Option[String], date: Date, engine: Engine, description: Option[String] = None) =
    new FocusedReport(title, List(engine.asRequirement), description)
  def focusedReport(title: Option[String], pathWithoutReport: List[Reportable], description: Option[String] = None) =
    new FocusedReport(title, pathWithoutReport, description)
  def traceReport(title: Option[String], traceItems: List[TraceItem[Engine, Any, Any, AnyConclusion]], description: Option[String] = None) =
    new TraceReport(title, traceItems, description)

  def htmlAndRenderedContext(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String]): (String, RenderContext) = {
    val urlMap = UrlMap() ++ report.urlMapPaths
    val iconUrl = Strings.url(urlMap.rootUrl, report.titleString, "")
    val renderContext = RenderContext(urlMap, new Date(), iconUrl)
    (html(report, engine, renderContext), renderContext)
  }
  def html(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String]): String =
    htmlAndRenderedContext(report, engine)._1

  def html(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String], renderContext: RenderContext): String =
    Lists.traversableToStartChildEnd(report.reportPaths).foldLeft("") { case (html, (path, cse)) => html + engine(renderContext, path, cse) }

  def htmlAndRenderedContext(title: Option[String], traceItems: List[TraceItem[Engine, Any, Any, AnyConclusion]], description: Option[String] = None)(implicit ldp: CddDisplayProcessor): (String, RenderContext) = {
    val report = new TraceReport(title, traceItems, description)
    val urlMap = UrlMap() ++ report.urlMapPaths
    val iconUrl = Strings.url(urlMap.rootUrl, report.titleString, "")
    val renderContext = RenderContext(urlMap, new Date(), iconUrl)
    (html(report, HtmlRenderer.traceReportSingleItemRenderer, renderContext), renderContext)
  }

  def htmlFromTrace(title: String, traceItems: List[TraceItem[Engine, Any, Any, AnyConclusion]], description: Option[String] = None)(implicit ldp: CddDisplayProcessor): String =
    htmlAndRenderedContext(Some(title), traceItems, description)._1

  def rendererFor(report: Report) =
    report match {
      case r: DocumentAndEngineReport => HtmlRenderer.engineAndDocumentsSingleItemRenderer
      case r: TraceReport => HtmlRenderer.traceReportSingleItemRenderer
      case r: FocusedReport => r.focusPath.head match {
        case e: EngineRequirement[_, _] => HtmlRenderer.engineReportSingleItemRenderer
        case uc: UseCase[_, _] => HtmlRenderer.useCaseOrScenarioReportRenderer
        case s: Scenario[_, _] => HtmlRenderer.useCaseOrScenarioReportRenderer
      }
    }

  def htmlAndRenderedContext(report: Report): (String, RenderContext) = htmlAndRenderedContext(report, rendererFor(report))
}

trait Report extends TitledReportable {
  def title: Option[String]
  def description: Option[String]
  def reportPaths: List[List[Reportable]]
  def urlMapPaths: List[List[Reportable]] = reportPaths
}

trait ReportWriter {
  def print(url: String, main: Option[Reportable], html: String)
}

class FileReportWriter extends ReportWriter {
  def print(url: String, main: Option[Reportable], html: String) {
    val prefix = "file:/"
    if (url.startsWith(prefix)) {
      val path = url.substring(prefix.length())
      Files.printToFile(new File(path))((pw) =>
        pw.print(html))
    } else throw new IllegalArgumentException("Url is " + url)
  }
}

class MemoryReportWriter extends ReportWriter {
  var map: Map[String, (Option[Reportable], String)] = Map()
  def print(url: String, main: Option[Reportable], html: String) {
    if (map.contains(url)) throw new IllegalArgumentException(url + " is already in map")
    map = map + (url -> (main, html))
  }

}
class ReportOrchestrator(rootUrl: String, title: String, engines: List[Engine], date: Date = new Date, reportWriter: ReportWriter = new FileReportWriter) {
  import EngineTools._
  import Strings._
  val rootReport = Report.documentAndEngineReport(Some(title), date, engines)
  val engineReports = engines.foldLeft(List[Report]())((list, e) => Report.engineReport(Some("title"), date, e) :: list).reverse
  val urlMap = UrlMap(rootUrl) ++ rootReport.urlMapPaths
  val iconUrl = Strings.url(rootUrl, title, "index.html")
  val renderContext = RenderContext(urlMap, date, iconUrl)

  def makeReports =
    Exceptions({
      val t = rootReport.reportPaths
      reportWriter.print(iconUrl, None, Report.html(rootReport, HtmlRenderer.engineAndDocumentsSingleItemRenderer, renderContext))

      for (e <- engines; path: List[Reportable] <- e.asRequirement.pathsIncludingSelf.toList) {
        val r = path.head
        val url = urlMap(r)
        val report = Report.focusedReport(Some("title"), path)
        val renderer = HtmlRenderer.rendererFor(r)
        val actualPathToConclusion = pathToConclusion(path)
        val newRenderContext = renderContext.copy(pathToConclusion = actualPathToConclusion)
        val html = Report.html(report, renderer, newRenderContext)
        reportWriter.print(url, Some(r), html)
      }
    }, (e) => { System.err.println("Failed in make report"); e.printStackTrace; if (e.getCause != null) { System.err.println("Cause"); e.getCause().printStackTrace; } })

  def pathToConclusion[Params, R](path: List[Reportable]): List[Reportable] = {
    def engineFromTestsFor(ed: EngineDescription[Params, R]) = {
      def fromEngine(e: Engine): List[EngineFromTests[Params, R]] = e match {
        case e: EngineFromTests[Params, R] if (e.asRequirement.eq(ed)) => List(e)
        case f: FoldingEngine[Params, R, _] => f.engines.collect { case e: EngineFromTests[Params, R] if (e.asRequirement.eq(ed)) => e }
        case d: DelegatedEngine[Params, R] => fromEngine(d.delegate)
        case _ => List()
      }
      engines.flatMap(fromEngine(_)).head
    }
    def pathFrom(e: EngineFromTests[Params, R], params: Params) = e.evaluator.findPathToConclusionWithParams(e.tree, params)

    path.head match {
      case s: Scenario[Params, R] => path.collect { case ed: EngineDescription[Params, R] => pathFrom(engineFromTestsFor(ed), s.params) }.head
      case _ => List()
    }
  }
}

case class SimpleReport(
  val title: Option[String],
  val description: Option[String],
  val nodes: List[Reportable],
  val textOrder: Int = Reportable.nextTextOrder) extends Report with NestedHolder[Reportable] {
  val reportPaths = pathsIncludingSelf.toList

  override def toString = s"Report(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
}

case class DocumentAndEngineReport(title: Option[String],
  val engines: Traversable[Engine],
  val description: Option[String] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends Report with NestedHolder[Reportable] {
  import EngineTools._
  import ReportableHelper._

  val documents = engines.flatMap(_.asRequirement.documents).toList.distinct
  val sortedEngines = engines.toList.sortBy(_.textOrder)
  val documentHolder = DocumentHolder(documents)
  val engineHolder = EngineHolder(sortedEngines)
  val nodes = List(documentHolder, engineHolder)
  private val thisAsPath: List[Reportable] = List(this)
  private val thisAndEngineHolderAsPath: List[Reportable] = List(engineHolder, this)
  private val shortenginePaths: List[List[Reportable]] = engines.flatMap(_.asRequirement match {
    case e: EngineDescription[_, _] => List(e :: thisAndEngineHolderAsPath)
    case f: FoldingEngineDescription[_, _, __] => {
      val head = (f :: thisAndEngineHolderAsPath)
      val tail = f.nodes.map(_ :: f :: thisAndEngineHolderAsPath);
      val result = head :: tail
      result
    }
  }).toList
  private val fullEnginePaths: List[List[Reportable]] = engines.flatMap(_.asRequirement.pathsIncludingTreeAndEngine(thisAsPath)).toList
  override val urlMapPaths = List(thisAsPath) ++ documentHolder.pathsFrom(thisAsPath) ++ fullEnginePaths
  val reportPaths = List(thisAsPath) ++ documentHolder.pathsIncludingSelf(thisAsPath) ++ List(thisAndEngineHolderAsPath) ++ shortenginePaths
  //    reportPaths ++ engines.map(_.asRequirement).flatMap((ed) => ed.pathsIncludingTreeAndEngine(engineHolder :: ed :: this :: Nil))

}

case class FocusedReport(title: Option[String],
  val focusPath: List[Reportable],
  val description: Option[String] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends Report {
  import EngineTools._
  import ReportableHelper._

  private val pathToFocus = focusPath :+ this
  private val pathsToFocus = Lists.decreasingList(pathToFocus).reverse
  val addDecisionTree = (path: List[Reportable]) => path match {
    case path @ (ed: EngineDescription[_, _]) :: tail => ed.tree.get.treePathsWithElseClause(path)
    case _ => List()
  }
  def childrenPaths(path: List[Reportable]): List[List[Reportable]] = path match {
    case (f: FoldingEngineDescription[_, _, _]) :: tail => f.nodes.flatMap {
      case ed: EngineDescription[_, _] => ed.pathsIncludingTreeAndEngine(path)
    }
    case (h: NestedHolder[Reportable]) :: tail => h.pathsFrom(path).toList
    case _ => List()
  }

  val reportPaths = pathsToFocus ::: childrenPaths(pathToFocus) ::: pathsToFocus.flatMap(addDecisionTree)

}
case class TraceReport(title: Option[String],
  val traceItems: List[TraceItem[Engine, Any, Any, AnyConclusion]],
  val description: Option[String] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends Report {
  import EngineTools._
  import ReportableHelper._
  import ReportableHelper._

  val reportPaths = {
    val thisPath = List(this)
    val traceItemPaths = traceItems.flatMap((traceItem) => traceItem.pathsIncludingSelf(thisPath))
    val result = traceItemPaths.flatMap {
      case path @ (ti: TraceItem[_, _, _, _]) :: tail =>
        path :: ((ti.main: @unchecked) match {
          case f: FoldingEngine[_, _, _] => List()
          case e: EngineFromTests[_, _] => e.tree.treePathsWithElseClause(path)
        })
    }
    thisPath :: result
  }
  def edsInTraceItem[Main, Params, Result, Evidence](traceItem: TraceItem[Main, Params, Result, Evidence]): List[EngineRequirement[_, _]] = {
    val head = traceItem.main match {
      case e: FoldingEngine[_, _, _] => e.asRequirement
      case e: EngineFromTests[_, _] => e.asRequirement
    }
    val tail = traceItem.nodes.flatMap(edsInTraceItem(_)).toList
    head :: tail
  }

  val eds = traceItems.flatMap(edsInTraceItem(_)).toList.distinct
  val feds: List[NestedHolder[Reportable]] = eds.collect { case fed: FoldingEngineDescription[_, _, _] => fed.asInstanceOf[NestedHolder[Reportable]] }
  val childEngines = feds.flatMap((fed) => fed.nodes.toSet)
  val edsWithoutChildEngines = eds.filter(!childEngines.contains(_))

  override val urlMapPaths = edsWithoutChildEngines.flatMap(_.pathsIncludingTreeAndEngine(List())) ::: traceItems.flatMap((traceItem) => traceItem.pathsIncludingSelf)
}

/** A reportable with a wrapper is used in the reports when making a projection of an original report. This allows the correct urls to be determined */
trait ReportableWrapper extends Reportable {
  def delegate: Option[Reportable]

}