package org.legacycdd.legacy

import org.cddcore.engine._

trait ReporterToHtml {
  type Rep <: LegacyReporter[_, _]
  def categorisationEngine: Engine[_]
  def replacementEngine: Engine[_]
  def apply(rep: Rep): String
}

class MemoryReporterToHtml[ID, R](categorisationEngine: Engine1[LegacyData[ID, String], String], replacementEngine: Engine[R], rep: MemoryReporter[ID, R]) {
  type Rep = MemoryReporter[ID, _]
  import Reportable._
  import Renderer._
  import HtmlRenderer._
  import PathUtils._
  class LegacyIfThenHtmlPrinter(e: Engine[_]) extends HtmlIfThenPrinter {
    import Renderer._
    import HtmlForIfThenPrinter._
    import Strings._

    override def resultPrint(path: ReqList, conclusion: Conclusion, resultClassName: String) = {
      val count = rep.itemsFor(conclusion).size
      s"<div class='$resultClassName'>${nbsp(indent(path))}<span class='keyword'>then&#160;</span><div class='conclusion'>" + count + s" ${htmlEscape(conclusion.code.pretty)}</div><!-- conclusion --></div><!-- $resultClassName -->\n"
    }

  }

  def legacyDecisionTreeConfig(rep: Rep) = RenderAttributeConfigurer[Engine[_]]("Engine",
    (reportableToUrl, urlMap, path, e, stringTemplate) => stringTemplate.setAttribute("decisionTree", e.toStringWith(new LegacyIfThenHtmlPrinter(e))))

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
  val idRow = "<tr class='legacyTR'><td class='title'>ID</td><td class='value'>$if(legacyItemId)$$legacyItemId$$endif$</td></tr>"
  val actualRow = "<tr><td class='title'>Actual</td><td class='value'>$if(actual)$$actual$$endif$</td></tr>"
  def legacyItemSummary: StringRenderer =
    ("LegacyItem",
      "<div class='legacyItem'><div class='legacyItemTable'>" + table("legacyItemTable", idRow, paramsRow,  expectedRow, actualRow) + "</div></div>")

  class EngineConclusionLegacyWalker(maxItems: Int = 10) extends ReportWalker {
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

  def legacyHtml(rep: Rep, rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, false, new EngineConclusionLegacyWalker).
    configureAttribute(legacyDecisionTreeConfig(rep), legacyConclusionConfig, legacyItemConfig).
    configureReportableHolder(reportTemplate, engineTemplate, conclusionSummary).
    configureReportable(legacyItemSummary)

  def legacyConclusionHtml(rep: Rep, rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, false, new EngineConclusionLegacyWalker).
    configureAttribute(legacyDecisionTreeConfig(rep), legacyConclusionConfig, legacyItemConfig).
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

  def createReport(reportableToUrl: FileSystemReportableToUrl = new FileSystemReportableToUrl) {
    import PathUtils._
    val report = new Report("Legacy run", None, categorisationEngine, replacementEngine)
    val urlMap = reportableToUrl.makeUrlMapWithDecisionsAndConclusions(report)
    report.walkWithPath((path: ReportableList) => path.head match {
      case r: Report => {
        val htmlMaker = legacyHtml(rep, None, Set())
        val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, r)
        Files.printToFile(reportableToUrl.file(path))(_.println(html))
      }

      case e: Engine[_] =>
        e.walkDecisionsAndConclusion((cd: ConclusionOrDecision) => cd match {
          case c: Conclusion =>
            val e = findEngine(path)
            val htmlMaker = legacyConclusionHtml(rep, None, Set())
            val html = htmlMaker.render(new SimpleReportableToUrl, urlMap, new Report("Legacy: " + e.titleOrDescription(""), None, e))
            Files.printToFile(reportableToUrl.file(c :: path))(_.println(html))
          case d: Decision => ;
        })
      case _ =>
    })
  }

}