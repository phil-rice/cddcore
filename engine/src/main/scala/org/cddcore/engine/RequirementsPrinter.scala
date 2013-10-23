package org.cddcore.engine

import java.text.MessageFormat

import java.io.StringReader
import scala.io.Source
import org.antlr.stringtemplate._
import org.joda.time.format.DateTimeFormat

case class ResultAndIndent(indent: Int = 1, result: String = "")

class SimpleRequirementsPrinter extends RequirementsFolder[ResultAndIndent] {
  def titleString(i: Int, r: Requirement) = s"<h${i}>${r.titleString}</h${i}>\n"
  def descriptionString(r: Requirement) = r.description.collect { case (d) => s"<p>$d</p>\n" }.getOrElse("")

  def holderFnStart = (acc, h) =>
    ResultAndIndent(acc.indent + 1, acc.result + titleString(acc.indent, h) + descriptionString(h))

  def childFn = (acc, c: Requirement) =>
    ResultAndIndent(acc.indent, acc.result + titleString(acc.indent, c) + descriptionString(c))

  def holderFnEnd = (acc, h) =>
    ResultAndIndent(acc.indent - 1, acc.result)
}

trait RequirementsPrinterTemplate {
  def reportStart: String
  def projectStart: String
  def engineStart: String
  def useCaseStart: String
  def scenario: String
  def useCaseEnd: String
  def engineEnd: String
  def projectEnd: String
  def reportEnd: String
}

class HtmlRequirementsPrinterTemplate extends RequirementsPrinterTemplate {
  private val title = "$title$"
  private val description = "$if(description)$<p>$description$</p>$endif$"
  private val date = "$if(reportDate)$<hr /><div class='dateTitle'>$reportDate$</div><hr /><div>$reportDate$</div>$endif$"
  private def titleAndDescription(clazz: String) = s"<div class='$clazz'>" + title + " " + description + "</div>"

  private val expectedRow = "<tr><td class='title'>Expected</td><td class='value'>$if(expected)$$expected$$endif$</td></tr>"
  private val codeRow = "$if(code)$<tr><td class='title'>Code</td><td class='value'>$code$</td></tr>$endif$"
  private val becauseRow = "$if(because)$<tr><td class='title'>Because</td><td class='value'>$because$</td></tr>$endif$"
  private val nodesCountRow = "<tr><td class='title'>Nodes</td><td class='value'>$decisionTreeNodes$</td></tr>"
  private val paramsRow = "<tr><td class='title'>Parameter</td><td class='value'>$params: {p|$p$}; separator=\"<hr /> \"$</td></tr>"
  private val useCasesRow = "$if(usecaseCount)$<tr><td class='title'>Usecases</td><td class='value'>$usecaseCount$</td></tr>$endif$"
  private val summariesRow = "$if(summariesCount)$<tr><td class='title'>Scenarios</td><td class='value'>$summariesCount$</td></tr>$endif$"
  private val refsRow = "$if(references)$<tr><td class='title'>References</td><td class='value'>$references: {r|$r$}; separator=\", \"$</td></tr>$endif$"

  def reportStart =
    "<!DOCTYPE html><html><head><title>CDD Report: $title$</title><style>" + Files.getFromClassPath(getClass, "cdd.css") + "\n</style></head>\n" +
      "<body>" +
      "<div class='report'>" +
      "<div class='topRightBox'>" + Files.getFromClassPath(getClass, "OurAdvert.xml") + "</div>" +
      "<div class='reportTopBox'>" +
      "<div class='reportTitle'>Report name</div>" +
      "<div class='reportText'>" + title + " " + description + "</div>" +
      "<div class='reportTitle'>Report date</div>" +
      "<div class='reportText'>$reportDate$</div></div>" +
      "\n"
  def table(clazz: String, rows: String*) =
    s"<table class='$clazz'>${rows.mkString("")}</table>"
  def projectStart = "<div class='project'><div class='projectText'><b>Project: $title$</b> " + description + "</div>\n"
  def engineStart = "<div class='engine'>" + titleAndDescription("engineText") + table("engineTable", refsRow, useCasesRow, nodesCountRow) + "\n"
  def useCaseStart = "<div class='usecase'>" + titleAndDescription("usecaseText") + table("usecaseTable", refsRow, summariesRow) + "\n"
  def scenario = "<div class='scenario'>" + titleAndDescription("scenarioText") +
    table("scenarioTable",
      refsRow,
      paramsRow,
      expectedRow,
      codeRow,
      becauseRow) + "</div>\n"
  def useCaseEnd = "</div> <!-- UseCase -->\n"
  def engineEnd = "</div> <!-- Engine -->\n"
  def projectEnd = "</div> <!-- Project -->\n"
  def reportEnd = "</div></body></html>"
}

object RequirementsPrinter {

  def apply(reportStart: String, projectStart: String, engineStart: String, useCaseStart: String, scenario: String, useCaseEnd: String, engineEnd: String, projectEnd: String, reportEnd: String): RequirementsFolder[ResultAndIndent] =
    new StRequirementsPrinter(Map(
      "Report_start" -> reportStart,
      "Project_start" -> projectStart,
      "Engine_start" -> engineStart,
      "UseCase_start" -> useCaseStart,
      "Scenario" -> scenario,
      "UseCase_end" -> useCaseEnd,
      "Engine_end" -> engineEnd,
      "Project_end" -> projectEnd,
      "Report_end" -> reportEnd))
  def apply(builderStart: String, useCaseStart: String, scenario: String, useCaseEnd: String, builderEnd: String): RequirementsFolder[ResultAndIndent] =
    new StRequirementsPrinter(Map(
      "engine_start" -> builderStart,
      "UseCase_start" -> useCaseStart,
      "Scenario" -> scenario,
      "UseCase_end" -> useCaseEnd,
      "engine_end" -> builderEnd))

  private lazy val htmlTemplate = new HtmlRequirementsPrinterTemplate
  def html = apply(htmlTemplate)

  def apply(r: RequirementsPrinterTemplate): RequirementsFolder[ResultAndIndent] =
    apply(
      r.reportStart,
      r.projectStart,
      r.engineStart,
      r.useCaseStart,
      r.scenario,
      r.useCaseEnd,
      r.engineEnd,
      r.projectEnd,
      r.reportEnd)
}

class StRequirementsPrinter(nameToTemplate: Map[String, String], startPattern: String = "{0}_start", childPattern: String = "{0}", endPattern: String = "{0}_end", dateFormat: String = "HH:mm EEE MMM d yyyy") extends RequirementsFolder[ResultAndIndent] {
  private val formatter = DateTimeFormat.forPattern(dateFormat);
  val renderer = new Renderer
  val refRenderer = new ReferenceRenderer
  val templateMap = nameToTemplate.mapValues((s) => {
    val t = new StringTemplate(s)
    t.registerRenderer(classOf[ValueForRender], renderer)
    t.registerRenderer(classOf[Reference], refRenderer)
    t
  })
  def render(acc: ResultAndIndent, r: Requirement, pattern: String, preIndentFn: (Int) => Int = (i) => i, postIndentFn: (Int) => Int = (i) => i) = {
    val preIndent = preIndentFn(acc.indent)
    val name = MessageFormat.format(pattern, r.templateName)
    val template = templateMap(name)
    template.setAttribute("indent", Integer.toString(preIndent))
    template.setAttribute("description", r.description.collect { case d => ValueForRender(d) }.getOrElse(null))
    template.setAttribute("title", ValueForRender(r.titleString))
    for (ref <- r.references)
      template.setAttribute("references", ref)

    r match {
      case holder: RequirementHolder =>
        r.templateName match {
          case "UseCase" => template.setAttribute("summariesCount", holder.children.size)
          case _ => ;
        }
      case _ => ;
    }
    r match {
      case e: Engine =>
        template.setAttribute("usecaseCount", e.children.size)
        template.setAttribute("decisionTreeNodes", e.decisionTreeNodes)

      case r: Report =>
        template.setAttribute("reportDate", formatter.print(System.currentTimeMillis()))
      case t: Test =>
        template.setAttribute("code", ValueForRender(t.optCode.collect { case c => c.description } getOrElse (null)))
        template.setAttribute("expected", ValueForRender(t.expected.getOrElse("")))
        template.setAttribute("paramCount", t.params.size)
        template.setAttribute("because", ValueForRender(t.because.collect { case c => c.description } getOrElse (null)))
        for (p <- t.params)
          template.setAttribute("params", ValueForRender(t.paramPrinter(p)))
      case _ =>
    }

    val result = template.toString
    ResultAndIndent(postIndentFn(preIndent), acc.result + result)
  }

  def holderFnStart = (acc, h) => render(acc, h, startPattern, postIndentFn = (i) => i + 1)
  def childFn = (acc, c: Requirement) => render(acc, c, childPattern)
  def holderFnEnd = (acc, h) => render(acc, h, endPattern, preIndentFn = (i) => i - 1)

}

case class ValueForRender(value: Any) {
  override def toString = if (value == null) "" else value.toString
}

class Renderer extends AttributeRenderer {
  def toString(o: Object): String = toString(o, "")
  def toString(o: Object, format: String): String = {
    val result = if (o == null)
      null
    else
      Strings.htmlEscape(o.toString())
    result
  }

}
class ReferenceRenderer extends AttributeRenderer {
  def toString(o: Object): String = toString(o, "")
  def toString(o: Object, format: String): String = {
    val ref = o.asInstanceOf[Reference]
    ref.document match {
      case Some(d) => d.url match {
        case Some(url) => s"<a href='$url'>${Strings.htmlEscape(d.titleString)}</a>"
        case _ => Strings.htmlEscape(d.titleString)
      }
      case None => ref.ref
    }
  }

}




