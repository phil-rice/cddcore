package org.cddcore.engine

import java.io.File
import org.cddcore.engine.tests.CddRunner
import org.scalatest.junit.JUnitRunner

class WebPagesCreator(project: Project, reportableToUrl: FileSystemReportableToUrl = new FileSystemReportableToUrl) {
  import Reportable._
  import Renderer._
  val report = junitReport(project)
  val urlMap = reportableToUrl.makeUrlMap(report)
  val rootUrl = reportableToUrl.url(List(project, report))

  def junitReport(r: Requirement*) = {
    val last = r.last
    Report("Website", last)
  }

  def print(path: ReportableList, html: String) {
    val file = reportableToUrl.file(path)
    println(file)
    Files.printToFile(file)((p) => p.append(html))
  }

  def engine(path: ReportableList) = path.collect { case e: Engine => e }.head

  def create {
    report.walkWithPath((path) => {
      val r = path.head
      if (!urlMap.contains(r))
        throw new IllegalStateException
      val optHtml = r match {
        //        case r: Report => Some(HtmlRenderer.reportHtml(rootUrl).render(reportableToUrl, urlMap, r))
        case p: Project => Some(HtmlRenderer.projectHtml(rootUrl).render(reportableToUrl, urlMap, Report("Project: "+p.projectTitle, p)))
        case e: Engine => Some(HtmlRenderer.engineHtml(rootUrl).render(reportableToUrl, urlMap, Report("Engine: " + e.titleOrDescription("<Unnamed>"), engine(path))))
        case u: RequirementAndHolder => Some(HtmlRenderer.usecaseHtml(rootUrl, restrict = path.toSet ++ u.children).render(reportableToUrl, urlMap, Report("Usecase: "+ u.titleOrDescription("<Unnamed>"), engine(path))))
        case t: Test =>
          Some(HtmlRenderer.scenarioHtml(rootUrl, t, path.toSet).render(reportableToUrl, urlMap, Report("Scenario: "+ t.titleOrDescription("<Unnamed>"), engine(path))))
        case _ => None
      }
      optHtml match {
        case Some(html) => print(path, html)
        case _ => ;
      }
    })

  }

}