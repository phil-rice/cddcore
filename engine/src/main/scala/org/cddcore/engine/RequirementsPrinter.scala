package org.cddcore.engine

import java.text.MessageFormat

import java.io.StringReader
import scala.io.Source
import org.antlr.stringtemplate._
import org.joda.time.format.DateTimeFormat

object ReqPrintContext {
  def apply() = new ReqPrintContext(nameToRequirement = new NoNameForRequirement)
  def apply(nameToRequirement: NameForRequirement) = new ReqPrintContext(nameToRequirement = nameToRequirement)
}
case class ReqPrintContext(indent: Int = 1, result: String = "", nameToRequirement: NameForRequirement)

class SimpleRequirementsPrinter extends RequirementsFolder[ReqPrintContext] {
  def titleString(i: Int, r: Requirement) = s"<h${i}>${r.titleString}</h${i}>\n"
  def descriptionString(r: Requirement) = r.description.collect { case (d) => s"<p>$d</p>\n" }.getOrElse("")

  def holderFnStart = (acc, h) =>
    acc.copy(acc.indent + 1, acc.result + titleString(acc.indent, h) + descriptionString(h))

  def childFn = (acc, c: Requirement) =>
    acc.copy(acc.indent, acc.result + titleString(acc.indent, c) + descriptionString(c))

  def holderFnEnd = (acc, h) =>
    acc.copy(acc.indent - 1, acc.result)
}

trait RequirementsPrinterTemplate {
  import RequirementsPrinter._
  def reportStart: Renderer
  def projectStart: Renderer
  def engineStart: Renderer
  def useCaseStart: Renderer
  def scenario: Renderer
  def useCaseEnd: Renderer
  def engineEnd: Renderer
  def projectEnd: Renderer
  def reportEnd: Renderer
}

trait HtmlTemplate extends RequirementsPrinterTemplate {
  protected val title = "$title$"
  protected val description = "$if(description)$$description$$endif$"
  protected val date = "$if(reportDate)$<hr /><div class='dateTitle'>$reportDate$</div><hr /><div>$reportDate$</div>$endif$"
  protected def titleAndDescription(clazz: String, titlePattern: String) = s"<div class='$clazz'>" + MessageFormat.format(titlePattern, title) + " " + description + "</div>"

  protected val expectedRow = "<tr><td class='title'>Expected</td><td class='value'>$if(expected)$$expected$$endif$</td></tr>"
  protected val codeRow = "$if(code)$<tr><td class='title'>Code</td><td class='value'>$code$</td></tr>$endif$"
  protected val becauseRow = "$if(because)$<tr><td class='title'>Because</td><td class='value'>$because$</td></tr>$endif$"
  protected val nodesCountRow = "<tr><td class='title'>Nodes</td><td class='value'>$decisionTreeNodes$</td></tr>"
  protected val paramsRow = "<tr><td class='title'>Parameter</td><td class='value'>$params: {p|$p$}; separator=\"<hr /> \"$</td></tr>"
  protected val useCasesRow = "$if(usecaseCount)$<tr><td class='title'>Usecases</td><td class='value'>$usecaseCount$</td></tr>$endif$"
  protected val scenariosRow = "$if(scenariosCount)$<tr><td class='title'>Scenarios</td><td class='value'>$scenariosCount$</td></tr>$endif$"
  protected val refsRow = "$if(references)$<tr><td class='title'>References</td><td class='value'>$references: {r|$r$}; separator=\", \"$</td></tr>$endif$"

  protected val usecaseHtml = "<div class='usecase'>" + titleAndDescription("usecaseText", "Use Case: {0}") + table("usecaseTable", refsRow, scenariosRow) + "\n"
  protected val engineHtml = "<div class='engine'>" + titleAndDescription("engineText", "Engine: {0}") + table("engineTable", refsRow, useCasesRow, nodesCountRow) + "\n"
  protected val scenarioHtml = "<div class='scenario'>" + titleAndDescription("scenarioText", "Scenario: {0}") +
    table("scenarioTable",
      refsRow,
      paramsRow,
      expectedRow,
      codeRow,
      becauseRow) + "</div><!-- scenario -->\n"

  def table(clazz: String, rows: String*) = {
    val result = s"<table class='$clazz'>${rows.mkString("")}</table>"
    result
  }
}

trait HtmlReportTemplate extends HtmlTemplate {
  def reportStart =
    Renderer("<!DOCTYPE html><html><head><title>CDD Report: $title$</title><style>" + Files.getFromClassPath(getClass, "cdd.css") + "\n</style></head>\n" +
      "<body>" +
      "<div class='report'>" +
      "<div class='topLine'>"+
      "<div class='cddLogo'><img src='http://img24.imageshack.us/img24/4325/gp9j.png'  alt='CDD'/></div>" +
      "<div class='advertBox'>" + Files.getFromClassPath(getClass, "OurAdvert.xml") + "</div>" +
      "<div class='reportTopBox'>" +
      "<div class='reportTitle'>Report name</div>" +
      "<div class='reportText'>" + title + " " + description + "</div>" +
      "<div class='reportTitle'>Report date</div>" +
      "<div class='reportText'>$reportDate$</div></div></div>" +
      "\n")
  def reportEnd = Renderer("</div><!-- report -->\n</body></html>")
}

trait HtmlProjectTemplate extends HtmlTemplate {
  def projectStart = Renderer("<div class='project'><div class='projectText'><b>Project: $title$</b> " + description + "</div>\n")
  def projectEnd = Renderer("</div> <!-- Project -->\n")

}

trait HtmlUseCaseScenario extends HtmlTemplate {
  def useCaseStart = Renderer(usecaseHtml)
  def scenario = Renderer(scenarioHtml)
  def useCaseEnd = Renderer("</div> <!-- UseCase -->\n")

}

class HtmlRequirementsPrinterTemplate extends HtmlReportTemplate with HtmlProjectTemplate with HtmlUseCaseScenario {

  def engineStart = Renderer(engineHtml)
  def engineEnd = Renderer("</div> <!-- Engine -->\n")

}

class HtmlDecisionTreePrinterTemplate(test: Option[Test]) extends HtmlReportTemplate with HtmlProjectTemplate {

  class UseCaseForDTRenderer(template: String) extends Renderer {
    val superRenderer = Renderer(template)
    def render(nameForRequirement: NameForRequirement, path: List[Requirement], indent: Int, r: Requirement, pattern: String): String = {
      (r, test) match {
        case (holder: RequirementHolder, Some(t)) => if (holder.children.contains(t)) superRenderer.render(nameForRequirement,
          path, indent, r, pattern)
        else ""; case _ => ""
      }
    }
  }
  class ScenarioForDTRenderer extends Renderer {
    val superRenderer = Renderer(scenarioHtml)
    def render(nameForRequirement: NameForRequirement, path: List[Requirement], indent: Int, r: Requirement, pattern: String): String = {
      (r, test) match { case (test: Test, Some(t)) => if (test == t) superRenderer.render(nameForRequirement, path, indent, r, pattern) else ""; case _ => "" }
    }
  }

  class EngineForDTRenderer(template: String) extends Renderer {
    def render(nameForRequirement: NameForRequirement, path: List[Requirement], indent: Int, r: Requirement, pattern: String): String = {
    val superRenderer = Renderer(template)
      val dt = r match {
        case e: Engine =>
          e.toStringWith(test match {
            case Some(t) => new HtmlWithTestIfThenPrinter(t, nameForRequirement)
            case _ => new HtmlIfThenPrinter(nameForRequirement)
          })

      }
      s"<div class='decisionTree'>$dt</div><!--decisionTree -->\n<div class='useCaseAndScenario'>\n" + superRenderer.render(nameForRequirement, path, indent, r, pattern)
    }
  }
  def engineStart =  new EngineForDTRenderer(engineHtml)
  def useCaseStart = new UseCaseForDTRenderer(usecaseHtml)
  def scenario = new ScenarioForDTRenderer
  def useCaseEnd = new UseCaseForDTRenderer("</div><!-- useCase -->\n")
  def engineEnd =Renderer("</div><!-- engine --> </div><!--useCaseAndScenario --> \n")
}

object Renderer {
  val renderer = new ValueForRenderer
  val refRenderer = new ReferenceRenderer
  private val dateFormat: String = "HH:mm EEE MMM d yyyy"
  val dateFormatter = DateTimeFormat.forPattern(dateFormat);
  implicit def apply(s: String) = StRenderer(s)
}

trait Renderer {
  def render(nameForRequirement: NameForRequirement, path: List[Requirement], indent: Int, r: Requirement, pattern: String): String
}

case class StRenderer(template: String) extends Renderer {
  import Renderer._
  val stringTemplate = new StringTemplate(template)
  stringTemplate.registerRenderer(classOf[ValueForRender], renderer)
  stringTemplate.registerRenderer(classOf[Reference], refRenderer)

  def render(nameForRequirement: NameForRequirement, path: List[Requirement], indent: Int, r: Requirement, pattern: String): String = {
    stringTemplate.reset()
    stringTemplate.setAttribute("indent", Integer.toString(indent))
    stringTemplate.setAttribute("description", r.description.collect { case d => ValueForRender(d) }.getOrElse(null))
    stringTemplate.setAttribute("title", ValueForRender(r.titleString))
    for (ref <- r.references)
      stringTemplate.setAttribute("references", ref)

    r match {
      case holder: RequirementHolder =>
        r.templateName match {
          case "UseCase" => stringTemplate.setAttribute("scenariosCount", holder.children.size)
          case _ => ;
        }
      case _ => ;
    }
    r match {
      case e: Engine =>
        stringTemplate.setAttribute("usecaseCount", e.children.size)
        stringTemplate.setAttribute("decisionTreeNodes", e.decisionTreeNodes)

      case r: Report =>
        stringTemplate.setAttribute("reportDate", dateFormatter.print(System.currentTimeMillis()))
      case t: Test =>
        stringTemplate.setAttribute("code", ValueForRender(t.optCode.collect { case c => c.pretty } getOrElse (null)))
        stringTemplate.setAttribute("expected", ValueForRender(t.expected.getOrElse("")))
        stringTemplate.setAttribute("paramCount", t.params.size)
        stringTemplate.setAttribute("because", ValueForRender(t.because.collect { case c => c.pretty } getOrElse (null)))
        for (p <- t.params)
          stringTemplate.setAttribute("params", ValueForRender(t.paramPrinter(p)))
      case _ =>
    }

    val result = stringTemplate.toString
    result

  }
}

object RequirementsPrinter {

  def apply(reportStart: Renderer, projectStart: Renderer, engineStart: Renderer, useCaseStart: Renderer, scenario: Renderer, useCaseEnd: Renderer, engineEnd: Renderer, projectEnd: Renderer, reportEnd: Renderer): RequirementsFolderWithPath[ReqPrintContext] =
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
  def apply(builderStart: Renderer, useCaseStart: Renderer, scenario: Renderer, useCaseEnd: Renderer, builderEnd: Renderer): RequirementsFolderWithPath[ReqPrintContext] =
    new StRequirementsPrinter(Map(
      "Engine_start" -> builderStart,
      "UseCase_start" -> useCaseStart,
      "Scenario" -> scenario,
      "UseCase_end" -> useCaseEnd,
      "Engine_end" -> builderEnd))

  private lazy val htmlTemplate = new HtmlRequirementsPrinterTemplate
  def html = apply(htmlTemplate)

  def decisionTree(test: Option[Test] = None) = apply(new HtmlDecisionTreePrinterTemplate(test))

  def apply(r: RequirementsPrinterTemplate): RequirementsFolderWithPath[ReqPrintContext] =
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

class StRequirementsPrinter(nameToRenderer: Map[String, Renderer], startPattern: String = "{0}_start", childPattern: String = "{0}", endPattern: String = "{0}_end") extends RequirementsFolderWithPath[ReqPrintContext] {
  import StRenderer._

  def render(nameForRequirement: NameForRequirement, path: List[Requirement], indent: Int, r: Requirement, pattern: String): String = {
    val name = MessageFormat.format(pattern, r.templateName)
    val renderer = nameToRenderer(name)
    renderer.render(nameForRequirement, path, indent, r, pattern)
  }
  protected def render(acc: (List[Requirement], ReqPrintContext), r: Requirement, pattern: String, preIndentFn: (Int) => Int = (i) => i, postIndentFn: (Int) => Int = (i) => i): (List[Requirement], ReqPrintContext) = {
    val preIndent = preIndentFn(acc._2.indent)
    val result = render(acc._2.nameToRequirement, acc._1, preIndent, r, pattern)
    (acc._1, acc._2.copy(postIndentFn(preIndent), acc._2.result + result))
  }

  def holderFnStart = (acc, h) => render(acc, h, startPattern, postIndentFn = (i) => i + 1)
  def childFn = (acc, c: Requirement) => render(acc, c, childPattern)
  def holderFnEnd = (acc, h) => render(acc, h, endPattern, preIndentFn = (i) => i - 1)

}

class ValueForRender(value: Any) {
  override def toString = if (value == null) "" else value.toString
}

object ValueForRender {
  def apply(o: Object) = if (o == null) null else new ValueForRender(o)
}

class ValueForRenderer extends AttributeRenderer {
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




