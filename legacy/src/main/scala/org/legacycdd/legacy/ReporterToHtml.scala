package org.legacycdd.legacy

import org.cddcore.engine._
import scala.xml.XML

trait ReporterToHtml {
  type Rep <: LegacyReporter[_, _]
  def categorisationEngine: Engine[_]
  def replacementEngine: Engine[_]
  def apply(rep: Rep): String
}

class MemoryReporterToHtml[ID] {
  type Rep = MemoryReporter[ID, _]
  import Reportable._
  import HtmlRenderer._

  class LegacyIfThenHtmlPrinter(rep: Rep, e: Engine[_]) extends HtmlIfThenPrinter {
    import HtmlForIfThenPrinter._
    import Strings._

    override def resultPrint(path: ReqList, conclusion: Conclusion, resultClassName: String) = {
      val count = rep.countFor(e, conclusion)
      s"<div class='$resultClassName'>${nbsp(indent(path))}<span class='keyword'>then&#160;</span><div class='conclusion'>" + count + s" ${htmlEscape(conclusion.code.pretty)}</div><!-- conclusion --></div><!-- $resultClassName -->\n"
    }

  }

  def legacyDecisionTreeConfig(rep: Rep) = RenderAttributeConfigurer[Engine[_]]("Engine",
    (reportableToUrl, urlMap, path, e, stringTemplate) => stringTemplate.setAttribute("decisionTree", e.toStringWith(new LegacyIfThenHtmlPrinter(rep, e))))

  def legacyHtml(rep: Rep, rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, false).
    configureAttribute(legacyDecisionTreeConfig(rep)).
    configureReportableHolder(reportTemplate, engineTemplate,useCaseWithScenariosSummarisedTemplate).
    configureReportable(scenarioSummaryTemplate)

  def apply(rep: Rep): String = {
    val reportableToUrl = new SimpleReportableToUrl
    val report = new Report("Legacy run", None, rep.engines.toSeq: _*)
    val urlMap = reportableToUrl.makeUrlMap(report)
    val html = legacyHtml(rep, None, Set()).render(new SimpleReportableToUrl, urlMap, report)
    html
  }
}