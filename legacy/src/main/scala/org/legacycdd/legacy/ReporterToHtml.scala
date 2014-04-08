package org.legacycdd.legacy

import org.cddcore.engine._
import org.cddcore.reporting._
import org.cddcore.utilities._

trait ReporterToHtml {
  type Rep <: LegacyReporter[_, _]
  def categorisationEngine: EngineFull[_, _]
  def replacementEngine: EngineFull[_, _]
  def apply(rep: Rep): String
}

class MemoryReporterToHtml[ID, R, FullR](categorisationEngine: Engine1[LegacyData[ID, R], String], replacementEngine: Engine, rep: MemoryReporter[ID, R], maxItems: Int = 5) {
  type Rep = MemoryReporter[ID, _]

  import Reportable._
import EngineWithLogger._
import Renderer._
import HtmlRenderer._
import PathUtils._
  implicit val loggerDisplayProcessor: LoggerDisplayProcessor = replacementEngine.logger

  class LegacyIfThenHtmlPrinter(e: EngineBuiltFromTests[_], conclusions: Set[Conclusion], val reportableToUrl: ReportableToUrl, val urlMap: UrlMap, val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
    import HtmlForIfThenPrinter._
import Strings._
    def isSelected(c: Conclusion) = conclusions.contains(c)

    def holdsOneOf(c: Set[Conclusion]) = (conclusions & c).size > 0
    override def ifPrint(path: ReqList, decision: Decision): String =
      if (holdsOneOf(decision.allYesConclusion)) ifPrint(path, decision, "ifTrueOnPath") else // 
      if (holdsOneOf(decision.allNoConclusion)) ifPrint(path, decision, "ifFalseOnPath") else
        ifPrint(path, decision, "if")

    override def resultPrint(path: ReqList, conclusion: Conclusion): String =
      if (isSelected(conclusion))
        resultPrint(path, conclusion, "resultWithTest")
      else
        resultPrint(path, conclusion, "result")

    override def resultPrint(path: ReqList, conclusion: Conclusion, resultClassName: String) = {
      val scenarioHtml = conclusion.scenarios.map((s) => scenarioLink(urlMap, s, isSelected(s))).mkString
      val thenHtml = urlMap.get(conclusion) match {
        case Some(url) => s" <a href='$url'>${htmlEscape(conclusion.code.pretty)}</a>"
        case _ => htmlEscape(conclusion.code.pretty)
      }
      val count = rep.itemsFor(conclusion).size
      s"<div class='$resultClassName'>${nbsp(indent(path))}<span class='keyword'>then&#160;</span>$scenarioHtml<div class='conclusion'>" + count + thenHtml + "</div><!-- conclusion --></div><!-- $resultClassName -->\n"
    }
  }

  val idRow = "<tr class='legacyTR'><td class='title'>ID</td><td class='value'>$if(legacyItemId)$" + a("$legacyItemId$") + "$endif$</td></tr>"
  def descriptionRow =
    "$if(legacyDescription)$<tr class='legacyTR'><td class='title'>Description</td><td class='value'>$legacyDescription$</td></tr>$endif$"
  val actualRow = "<tr><td class='title'>Actual</td><td class='value'>$if(actual)$$actual$$endif$</td></tr>"

  class EngineConclusionLegacyWalker() extends ReportWalker {
    import Reportable._

    def foldWithPath[Acc](path: ReportableList, initial: Acc,
      startFn: (Acc, ReportableList) => Acc,
      childFn: (Acc, ReportableList) => Acc,
      endFn: (Acc, ReportableList) => Acc): Acc = {
      val head = path.head
      head match {
        case engine: EngineBuiltFromTests[_] =>
          var acc = startFn(initial, path)
          engine.walkDecisionsAndConclusion(engine.root, (cd: ConclusionOrDecision) => cd match {
            case c: Conclusion =>
              val cPath = c :: path
              acc = startFn(acc, cPath)
              for (item <- rep.itemsFor(c).reverse.take(maxItems))
                acc = childFn(acc, item :: cPath)
              acc = endFn(acc, cPath)
            case d: Decision => ;
          })
          acc = endFn(acc, path)
          acc
        case holder: ReportableHolder =>
          var acc = startFn(initial, path)
          for (c <- holder.children)
            acc = foldWithPath(c :: path, acc, startFn, childFn, endFn)
          acc = endFn(acc, path)
          acc
        case _ => childFn(initial, path)
      }
    }

  }

  def legacyDecisionTreeConfig(rep: Rep, conclusionNode: Set[Conclusion]) = RenderAttributeConfigurer[EngineBuiltFromTests[_]](Set(Renderer.engineFromTestsKey),
    (rc) => { import rc._; stringTemplate.setAttribute("decisionTree", r.toStringWith(new LegacyIfThenHtmlPrinter(r, conclusionNode, reportableToUrl, urlMap))) })

  def legacyConclusionConfig = RenderAttributeConfigurer[Conclusion](Set("CodeAndScenarios"), (rc) => {
    import rc._
    stringTemplate.setAttribute("conclusion", r.code.pretty)
    stringTemplate.setAttribute("conclusionCount", rep.itemsFor(r).size)
  })

  def conclusionSummary: StringRendererRenderer =
    ("CodeAndScenarios",
      "<div class='legacyConclusion'><div class='legacyConclusionTest'>" + a("Conclusion") + " $conclusion$ $conclusionCount$</div><!-- legacyConclusionTest -->" + table("legacyConclusionTable"),
      "</div><!-- legacyConclusion' -->")

  def legacyItemConfig = RenderAttributeConfigurer[LegacyItem[ID, R]](Set("LegacyItem"), (rendererContext) => {
    import EngineWithLogger._
import rendererContext._
import r._
    stringTemplate.setAttribute("legacyItemId", id)
    addParams(stringTemplate, "params", replacementEngine.logger, params)
    stringTemplate.setAttribute("expected", expected)
    stringTemplate.setAttribute("actual", actual)
    if (description.isDefined)
      stringTemplate.setAttribute("legacyDescription", description.get)
  })
  def legacyItemSummary: StringRenderer =
    ("LegacyItem",
      "<div class='legacyItem'><div class='legacyItemTable'>" + table("legacyItemTable", idRow, descriptionRow, paramsRow, expectedRow, actualRow) + "</div></div>")

  def legacyHtml(rep: Rep, rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(loggerDisplayProcessor, rootUrl, restrict, false, new EngineConclusionLegacyWalker).
    configureAttribute(legacyDecisionTreeConfig(rep, Set()), legacyConclusionConfig, legacyItemConfig).
    configureReportableHolder(reportTemplate, engineWithTestsTemplate, conclusionSummary).
    configureReportable(legacyItemSummary)

  def legacyConclusionHtml(rep: Rep, rootUrl: Option[String], restrict: ReportableSet = Set(), conclusion: Set[Conclusion]) = Renderer(loggerDisplayProcessor, rootUrl, restrict, false, new EngineConclusionLegacyWalker).
    configureAttribute(legacyDecisionTreeConfig(rep, conclusion), legacyConclusionConfig, legacyItemConfig).
    configureReportableHolder(reportTemplate, engineWithTestsTemplate, conclusionSummary).
    configureReportable(legacyItemSummary)

  def apply: String = {
    val report = new Report("Legacy run", categorisationEngine.asInstanceOf[Reportable], replacementEngine)
    val reportableToUrl = new SimpleReportableToUrl
    val urlMap = reportableToUrl.makeUrlMapWithDecisionsAndConclusions(report)
    val htmlMaker = legacyHtml(rep, None, Set())
    val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, report)
    html
  }

  def addToMap(reportableToUrl: ReportableToUrl, acc: UrlMap, path: ReportableList) = {
    val u = reportableToUrl.url(path);
    val withU = if (u.isDefined) acc + (path -> u.get) else acc
    withU
  }
  def makeUrlMap(reportableToUrl: ReportableToUrl, r: ReportableHolder): UrlMap =
    r.foldWithPath(reportableToUrl.makeUrlMap(r), ((acc: UrlMap, path) => {
      val withU = addToMap(reportableToUrl, acc, path)
      path.head match {
        case e: EngineBuiltFromTests[_] => e.fold(withU, new DecisionTreeFolder[UrlMap] {
          def apply(acc: UrlMap, c: Conclusion) = {
            val initial = addToMap(reportableToUrl, acc, c :: path)
            val in1 = initial.contains(c)
            val result = rep.itemsFor(c).foldLeft(initial)((acc, li) => addToMap(reportableToUrl, acc, li :: path))
            val in = result.contains(c)
            result
          }
          def apply(acc: UrlMap, d: Decision) = acc
        })
        case _ => withU
      }
    }))

  def createReport(reportableToUrl: FileSystemReportableToUrl = new FileSystemReportableToUrl) {
import PathUtils._
    val categorisationReportableHolder = categorisationEngine.asInstanceOf[ReportableHolder]
    val project = new Project("Legacy Run", categorisationReportableHolder, replacementEngine)
    val report = new Report("Legacy run", project)
    val urlMap = makeUrlMap(reportableToUrl, report)
    ReportCreator.fileSystem(loggerDisplayProcessor, report, reportableToUrl = reportableToUrl, optUrlMap = Some(urlMap)).create

    val rootUrl = urlMap.get(report)
    report.walkWithPath((path: ReportableList) => path.head match {
      case r: Report => {
        val htmlMaker = legacyHtml(rep, rootUrl, Set())
        val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, r)
        Files.printToFile(reportableToUrl.file(path))(_.println(html))
      }

      case e: EngineBuiltFromTests[_] =>
        e.walkDecisionsAndConclusion((cd: ConclusionOrDecision) => cd match {
          case c: Conclusion =>
            val e = findEngine(path)
            val htmlMaker = legacyConclusionHtml(rep, rootUrl, Set(e, cd) ++ rep.itemsFor(c) + categorisationReportableHolder + replacementEngine, Set(c))
            val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, new Report("Legacy: " + e.titleOrDescription(""), categorisationReportableHolder, replacementEngine))
            Files.printToFile(reportableToUrl.file(c :: path))(_.println(html))

            for (li <- rep.itemsFor(c)) {
              val htmlMaker = legacyConclusionHtml(rep, rootUrl, Set(e, li.categoriseConclusion, li.replacementConclusion) + li + categorisationReportableHolder + replacementEngine, Set(li.categoriseConclusion, li.replacementConclusion))
              val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, new Report("Legacy: " + e.titleOrDescription(""), categorisationReportableHolder, replacementEngine))
              Files.printToFile(reportableToUrl.file(li :: path))(_.println(html))
            }

          case d: Decision => ;
        })
      case _ =>
    })
  }

}