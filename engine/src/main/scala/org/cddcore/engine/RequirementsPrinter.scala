package org.cddcore.engine

import java.text.MessageFormat
import java.io.StringReader
import scala.io.Source
import org.antlr.stringtemplate.StringTemplate

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
  def builderStart: String
  def useCaseStart: String
  def scenario: String
  def useCaseEnd: String
  def builderEnd: String
}

object RequirementsPrinter {

  def apply(builderStart: String, useCaseStart: String, scenario: String, useCaseEnd: String, builderEnd: String): RequirementsFolder[ResultAndIndent] =
    new StRequirementsPrinter(Map(
      "builder_start" -> builderStart,
      "UseCase_start" -> useCaseStart,
      "Scenario" -> scenario,
      "UseCase_end" -> useCaseEnd,
      "builder_end" -> builderEnd))

  private val title = "$title$"
  private val description = "$if(description)$<p>$description$</p>$else$$endif$"
  private def titleAndDescription(clazz: String) = s"<div class='$clazz'>" + title + " " + description + "</div>"

  private val expectedRow = "<tr><td class='title'>Expected</td><td class='value'>$if(expected)$$expected$$endif$</td></tr>"
  private val codeRow = "$if(code)$<tr><td class='title'>Code</td><td class='value'>$code$</td></tr>$endif$"
  private val becauseRow = "$if(because)$<tr><td class='title'>Because</td><td class='value'>$because$</td></tr>$endif$"
  private val paramsRow = "<tr><td class='title'>Parameter</td><td class='value'>$params: {p|$p$}; separator=\"<hr />\"$</td></tr>"

  def html = apply(
    "<div class='builder'>" + titleAndDescription("builderText") + "\n",
    "<div class='usecase'>" + titleAndDescription("usecaseText") + "\n",
    "<div class='scenario'>" + titleAndDescription("scenarioText") +
      "<table class='scenarioTable'>" +
      paramsRow +
      expectedRow +
      codeRow +
      becauseRow +
      "</table></div>\n",
    "</div>\n",
    "</div>\n")

  def apply(r: RequirementsPrinterTemplate): RequirementsFolder[ResultAndIndent] =
    apply(
      r.builderStart,
      r.useCaseStart,
      r.builderStart,
      r.useCaseEnd,
      r.builderEnd)
}

class StRequirementsPrinter(nameToTemplate: Map[String, String], startPattern: String = "{0}_start", childPattern: String = "{0}", endPattern: String = "{0}_end") extends RequirementsFolder[ResultAndIndent] {

  val templateMap = nameToTemplate.mapValues((s) => new StringTemplate(s))

  def render(acc: ResultAndIndent, r: Requirement, pattern: String, preIndentFn: (Int) => Int = (i) => i, postIndentFn: (Int) => Int = (i) => i) = {
    val preIndent = preIndentFn(acc.indent)
    val name = MessageFormat.format(pattern, r.templateName)
    val template = templateMap(name)
    template.setAttribute("indent", Integer.toString(preIndent))
    template.setAttribute("description", r.description.getOrElse(null))
    template.setAttribute("title", r.titleString)
    r match {
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




