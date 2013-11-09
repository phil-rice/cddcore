package org.legacycdd.legacy

import org.cddcore.engine._
import org.cddcore.engine.ReportWalker

trait ReporterToHtml {
  type Rep <: LegacyReporter[_, _]
  def categorisationEngine: Engine[_]
  def replacementEngine: Engine[_]
  def apply(rep: Rep): String
}

class MemoryReporterToHtml[ID, R](categorisationEngine: Engine1[LegacyData[ID, String], String], replacementEngine: Engine[R], rep: MemoryReporter[ID, R], maxItems: Int = 5) {
  type Rep = MemoryReporter[ID, _]
  import Reportable._
  import Renderer._
  import HtmlRenderer._
  import PathUtils._

  class LegacyIfThenHtmlPrinter(e: Engine[_], conclusions: Set[Conclusion], val reportableToUrl: ReportableToUrl, val urlMap: UrlMap, val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
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
  val actualRow = "<tr><td class='title'>Actual</td><td class='value'>$if(actual)$$actual$$endif$</td></tr>"
  def legacyItemSummary: StringRenderer =
    ("LegacyItem",
      "<div class='legacyItem'><div class='legacyItemTable'>" + table("legacyItemTable", idRow, paramsRow, expectedRow, actualRow) + "</div></div>")

  class EngineConclusionLegacyWalker() extends ReportWalker {
    import Reportable._
    def foldWithPath[Acc](path: ReportableList, initial: Acc,
      startFn: (Acc, ReportableList) => Acc,
      childFn: (Acc, ReportableList) => Acc,
      endFn: (Acc, ReportableList) => Acc): Acc = {
      val head = path.head
      head match {
        case engine: Engine[_] =>
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

  def legacyDecisionTreeConfig(rep: Rep, conclusionNode: Set[Conclusion]) = RenderAttributeConfigurer[Engine[_]]("Engine",
    (reportableToUrl, urlMap, path, e, stringTemplate) =>
      stringTemplate.setAttribute("decisionTree", e.toStringWith(new LegacyIfThenHtmlPrinter(e, conclusionNode, reportableToUrl, urlMap))))

  def legacyConclusionConfig = RenderAttributeConfigurer[Conclusion]("CodeAndScenarios", (_, _, _, c, stringTemplate) => {
    stringTemplate.setAttribute("conclusion", c.code.pretty)
    stringTemplate.setAttribute("conclusionCount", rep.itemsFor(c).size)
  })

  def conclusionSummary: StringRendererRenderer =
    ("CodeAndScenarios",
      "<div class='legacyConclusion'><div class='legacyConclusionTest'>" + a("Conclusion") + " $conclusion$ $conclusionCount$</div><!-- legacyConclusionTest -->" + table("legacyConclusionTable"),
      "</div><!-- legacyConclusion' -->")

  def legacyItemConfig = RenderAttributeConfigurer[LegacyItem[ID, R]]("LegacyItem", (_, _, _, li, stringTemplate) => {
    stringTemplate.setAttribute("legacyItemId", li.id)
    addParams(stringTemplate, "params", replacementEngine.logger, li.params)
    stringTemplate.setAttribute("expected", li.expected)
    stringTemplate.setAttribute("actual", li.actual)
  })

  def legacyHtml(rep: Rep, rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, false, new EngineConclusionLegacyWalker).
    configureAttribute(legacyDecisionTreeConfig(rep, Set()), legacyConclusionConfig, legacyItemConfig).
    configureReportableHolder(reportTemplate, engineTemplate, conclusionSummary).
    configureReportable(legacyItemSummary)

  def legacyConclusionHtml(rep: Rep, rootUrl: Option[String], restrict: ReportableSet = Set(), conclusion: Set[Conclusion]) = Renderer(rootUrl, restrict, false, new EngineConclusionLegacyWalker).
    configureAttribute(legacyDecisionTreeConfig(rep, conclusion), legacyConclusionConfig, legacyItemConfig).
    configureReportableHolder(reportTemplate, engineTemplate, conclusionSummary).
    configureReportable(legacyItemSummary)

  def apply: String = {
    val report = new Report("Legacy run", None, categorisationEngine, replacementEngine)
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
    r.foldWithPath(List(), reportableToUrl.makeUrlMap(r), ((acc: UrlMap, path) => {
      val withU = addToMap(reportableToUrl, acc, path)
      path.head match {
        case e: Engine[_] => e.fold(withU, new DecisionTreeFolder[UrlMap] {
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
    val project = new Project("Legacy Run", categorisationEngine, replacementEngine)
    val report = new Report("Legacy run", None, project)
    val urlMap = makeUrlMap(reportableToUrl, report)
    ReportCreator.fileSystem(report, optUrlMap = Some(urlMap)).create

    val rootUrl = urlMap.get(report)
    report.walkWithPath((path: ReportableList) => path.head match {
      case r: Report => {
        val htmlMaker = legacyHtml(rep, rootUrl, Set())
        val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, r)
        Files.printToFile(reportableToUrl.file(path))(_.println(html))
      }

      case e: Engine[_] =>
        e.walkDecisionsAndConclusion((cd: ConclusionOrDecision) => cd match {
          case c: Conclusion =>
            val e = findEngine(path)
            val htmlMaker = legacyConclusionHtml(rep, rootUrl, Set(e, cd) ++ rep.itemsFor(c) + categorisationEngine + replacementEngine, Set(c))
            val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, new Report("Legacy: " + e.titleOrDescription(""), rootUrl, categorisationEngine, replacementEngine))
            Files.printToFile(reportableToUrl.file(c :: path))(_.println(html))

            for (li <- rep.itemsFor(c)) {
              val htmlMaker = legacyConclusionHtml(rep, rootUrl, Set(e, li.categoriseConclusion, li.replacementConclusion) + li + categorisationEngine + replacementEngine, Set(li.categoriseConclusion, li.replacementConclusion))
              val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, new Report("Legacy: " + e.titleOrDescription(""), rootUrl, categorisationEngine, replacementEngine))
              Files.printToFile(reportableToUrl.file(li :: path))(_.println(html))
            }

          case d: Decision => ;
        })
      case _ =>
    })
  }

}