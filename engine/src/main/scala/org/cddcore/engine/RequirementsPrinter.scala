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
  private val description = "$if(description)$<p>$description$</p>$else$$endif$"
  private val date = "$if(reportDate)$<hr /><div class='dateTitle'>$reportDate$</div><hr /><div>$reportDate$</div>$endif$"
  private def titleAndDescription(clazz: String) = s"<div class='$clazz'>" + title + " " + description + "</div>"

  private val expectedRow = "<tr><td class='title'>Expected</td><td class='value'>$if(expected)$$expected$$endif$</td></tr>"
  private val codeRow = "$if(code)$<tr><td class='title'>Code</td><td class='value'>$code$</td></tr>$endif$"
  private val becauseRow = "$if(because)$<tr><td class='title'>Because</td><td class='value'>$because$</td></tr>$endif$"
  private val paramsRow = "<tr><td class='title'>Parameter</td><td class='value'>$params: {p|$p$}; separator=\", \"$</td></tr>"
  private val refsRow = "<tr><td class='title'>References</td><td class='value'>$references: {r|$r$}; separator=\", \"$</td></tr>"

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
  def engineStart = "<div class='engine'>" + titleAndDescription("engineText") + table("engineTable", refsRow) + "\n"
  def useCaseStart = "<div class='usecase'>" + titleAndDescription("usecaseText") + table("usecaseTable", refsRow) + "\n"
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
  val templateMap = nameToTemplate.mapValues((s) => {
    val t = new StringTemplate(s)
    t.registerRenderer(classOf[String], renderer)
    t
  })
  def render(acc: ResultAndIndent, r: Requirement, pattern: String, preIndentFn: (Int) => Int = (i) => i, postIndentFn: (Int) => Int = (i) => i) = {
    val preIndent = preIndentFn(acc.indent)
    val name = MessageFormat.format(pattern, r.templateName)
    val template = templateMap(name)
    template.setAttribute("indent", Integer.toString(preIndent))
    template.setAttribute("description", r.description.getOrElse(null))
    template.setAttribute("title", r.titleString)
    for (ref <- r.references)
    	template.setAttribute("references", ref.titleString)
      
    r match {
      case r: Report =>
        template.setAttribute("reportDate", formatter.print(System.currentTimeMillis()))

      case t: Test =>
        template.setAttribute("code", t.optCode.collect { case c => c.description } getOrElse (null))
        template.setAttribute("expected", t.expected.getOrElse(""))
        template.setAttribute("paramCount", t.params.size)
        template.setAttribute("because", t.because.collect { case c => c.description } getOrElse (null))
        for (p <- t.params)
          template.setAttribute("params", t.paramPrinter(p))
      case _ =>
    }

    val result = template.toString
    ResultAndIndent(postIndentFn(preIndent), acc.result + result)
  }

  def holderFnStart = (acc, h) => render(acc, h, startPattern, postIndentFn = (i) => i + 1)
  def childFn = (acc, c: Requirement) => render(acc, c, childPattern)
  def holderFnEnd = (acc, h) => render(acc, h, endPattern, preIndentFn = (i) => i - 1)

}

class Renderer extends AttributeRenderer {
  def toString(o: Object): String = toString(o, "")
  def toString(o: Object, format: String): String = {
    val raw = if (o == null) "" else o.toString()
    raw.replace("&", "&amp;").replace("\"", "&quot;").replace("<", "&lt;").replace("&gt;", ">").replace("\n", "<br />")
  }

}




