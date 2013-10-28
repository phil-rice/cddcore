package org.cddcore.engine

import java.text.MessageFormat

import java.io.StringReader
import scala.io.Source
import org.antlr.stringtemplate._
import org.joda.time.format.DateTimeFormat

object HtmlRenderer {
  import Reportable._
  import Renderer._

  protected val title = "$title$"
  protected val description = "$if(description)$$description$$endif$"
  protected val date = "$if(reportDate)$<hr /><div class='dateTitle'>$reportDate$</div><hr /><div>$reportDate$</div>$endif$"
  protected def titleAndDescription(clazz: String, titlePattern: String) = s"<div class='$clazz'>" + "$if(url)$<a href='$url$'>$endif$" + MessageFormat.format(titlePattern, title) + "$if(url)$</a>$endif$ " + description + "</div>"

  protected def cddLogoImg = "<img src='http://img24.imageshack.us/img24/4325/gp9j.png'  alt='CDD'/>"

  protected val expectedRow = "<tr><td class='title'>Expected</td><td class='value'>$if(expected)$$expected$$endif$</td></tr>"
  protected val codeRow = "$if(code)$<tr><td class='title'>Code</td><td class='value'>$code$</td></tr>$endif$"
  protected val becauseRow = "$if(because)$<tr><td class='title'>Because</td><td class='value'>$because$</td></tr>$endif$"
  protected val nodesCountRow = "<tr><td class='title'>Nodes</td><td class='value'>$decisionTreeNodes$</td></tr>"
  protected val paramsRow = "<tr><td class='title'>Parameter</td><td class='value'>$params: {p|$p$}; separator=\"<hr /> \"$</td></tr>"
  protected val useCasesRow = "$if(childrenCount)$<tr><td class='title'>Usecases</td><td class='value'>$childrenCount$</td></tr>$endif$"
  protected val scenariosRow = "$if(childrenCount)$<tr><td class='title'>Scenarios</td><td class='value'>$childrenCount$</td></tr>$endif$"
  protected val refsRow = "$if(references)$<tr><td class='title'>References</td><td class='value'>$references: {r|$r$}; separator=\", \"$</td></tr>$endif$"

  def projectHtml(rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict).
    configureAttribute(Renderer.decisionTreeConfig(None)).
    configureReportableHolder(reportTemplate, engineTemplate, useCaseWithScenariosSummarisedTemplate).
    configureReportable(scenarioSummaryTemplate)

  def engineHtml(rootUrl: Option[String], test: Option[Test] = None, restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict).
    configureAttribute(Renderer.decisionTreeConfig(test)).
    configureReportableHolder(reportTemplate, projectTemplate, engineTemplate, useCaseWithScenariosSummarisedTemplate).
    configureReportable(scenarioSummaryTemplate)

  def usecaseHtml(rootUrl: Option[String], test: Option[Test] = None, restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict).
    configureAttribute(Renderer.decisionTreeConfig(test)).
    configureReportableHolder(reportTemplate, projectTemplate, engineTemplate, useCaseTemplate).
    configureReportable(scenarioTemplate)

  def scenarioHtml(rootUrl: Option[String], test: Test, restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict).
    configureAttribute(Renderer.decisionTreeConfig(Some(test))).
    configureReportableHolder(reportTemplate, projectTemplate, engineTemplate, useCaseTemplate).
    configureReportable(scenarioTemplate)

  val reportTemplate: StringRendererRenderer = ("Report", {
    "<!DOCTYPE html><html><head><title>CDD Report: $title$</title><style>" +
      Files.getFromClassPath(getClass, "cdd.css") + "\n</style></head>\n" +
      "<body>" +
      "<div class='report'>" +
      "<div class='topLine'>" +
      "<div class='cddLogo'>$if(rootUrl)$<a href='$rootUrl$'>$endif$" + cddLogoImg + "$if(rootUrl)$</a>$endif$</div>\n" +
      "<div class='advertBox'>" + Files.getFromClassPath(getClass, "OurAdvert.xml") + "</div>\n" +
      "<div class='reportTopBox'>\n" +
      "<div class='reportTitle'>Report name</div>\n" +
      "<div class='reportText'>" + title + " " + description + "</div>\n" +
      "<div class='reportTitle'>Report date</div>\n" +
      "<div class='reportDate'>$reportDate$</div>\n" +
      "</div><!--Report Top Box-->\n</div><!-- top Line -->\n"
  }, "</div><!-- report -->\n</body></html>")

  val projectTemplate: StringRendererRenderer =
    ("Project", "<div class='project'><div class='projectText'><b>Project: $title$</b> " + description + "</div>\n", "</div> <!-- Project -->\n")

  val engineTemplate: StringRendererRenderer =
    ("Engine", "<div class='engine'>" +
      "<div class='engineSummary'>" + titleAndDescription("engineText", "Engine {0}") + table("engineTable", refsRow, useCasesRow, nodesCountRow),

      "</div><!-- engineSummary -->" +
      "<div class='decisionTree'>\n$decisionTree$</div><!-- decisionTree -->\n" +
      "</div><!-- engine -->\n")
 

  val useCaseTemplate: StringRendererRenderer =
    ("UseCase",
      "<div class='usecase'>" + "<h4>$if(url)$<a href='$url$'>$endif$" + title + "</a></h4>\n$if(description)$<p>$description$</p>$endif$" + "\n",
      "</div><!-- useCase -->\n")

  val useCaseWithScenariosSummarisedTemplate: StringRendererRenderer =
    ("UseCase",
      "<div class='usecaseSummary'><h4> $if(url)$<a href='$url$'>$endif$" + title + "</a>\n",
      "</h4>$if(description)$<p>$description$</p>$endif$" + "</div><!-- usecaseSummary -->\n")

  val scenarioTemplate: StringRenderer = ("Scenario", "<div class='scenario'>" + titleAndDescription("scenarioText", "Scenario: {0}") +
    table("scenarioTable",
      refsRow,
      paramsRow,
      expectedRow,
      codeRow,
      becauseRow) + "</div><!-- scenario -->\n")
  val scenarioSummaryTemplate: StringRenderer = ("Scenario", "$if(url)$<a href='$url$'>$endif$<img src='" + HtmlForIfThenPrinter.normalScenarioIcon + "' />$if(url)$</a>$endif$")

  def table(clazz: String, rows: String*) = {
    val result = s"<table class='$clazz'>${rows.mkString("")}</table>"
    result
  }
}







