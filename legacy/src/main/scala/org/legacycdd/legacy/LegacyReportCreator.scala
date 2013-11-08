package org.legacycdd.legacy

import org.cddcore.engine._

class LegacyReportCreator[RtoUrl <: ReportableToUrl](project: RequirementAndHolder, title: String = null, val live: Boolean = false, val reportableToUrl: RtoUrl = new FileSystemReportableToUrl) {
  import Reportable._
  import Renderer._
  val report = Report(if (title == null) project.titleOrDescription("Unnamed") else title, project)
  val urlMap = reportableToUrl.makeUrlMap(report)
  val rootUrl = reportableToUrl.url(List(project, report))

  protected def engine(path: ReportableList) = path.collect { case e: Engine[_] => e }.head

  def htmlFor(path: ReportableList) = {
    val r = path.head
    if (!urlMap.contains(r))
      throw new IllegalStateException
    val optHtml = r match {
      case p: Project =>
        Some(HtmlRenderer(live).projectHtml(rootUrl).render(reportableToUrl, urlMap, Report("Project: " + p.titleOrDescription(ReportCreator.unnamed), p)))
      case e: Engine[_] => Some(HtmlRenderer(live).engineHtml(rootUrl).render(reportableToUrl, urlMap, Report("Engine: " + e.titleOrDescription(ReportCreator.unnamed), engine(path))))
      case u: RequirementAndHolder => Some(HtmlRenderer(live).usecaseHtml(rootUrl, restrict = path.toSet ++ u.children).render(reportableToUrl, urlMap, Report("Usecase: " + u.titleOrDescription(ReportCreator.unnamed), engine(path))))
      case t: Test =>
        val conclusion = PathUtils.findEngine(path).findConclusionFor(t.params)
        Some(HtmlRenderer(live).scenarioHtml(rootUrl, conclusion, t, path.toSet).render(reportableToUrl, urlMap, Report("Scenario: " + t.titleOrDescription(ReportCreator.unnamed), engine(path))))
      case _ => None
    }
    optHtml
  }

}