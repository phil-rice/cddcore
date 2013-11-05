package org.cddcore.engine

import java.text.MessageFormat
import org.antlr.stringtemplate.AttributeRenderer
import org.antlr.stringtemplate.StringTemplate
import org.joda.time.format.DateTimeFormat
import Reportable.ReportableList
import Reportable.ReportableSet
import org.cddcore.engine.tests.CddRunner
import java.io.File

object ReportCreator {
  def unnamed = "<Unnamed>"

  def fileSystem(project: Project, title: String = null) = new FileReportCreator(project, title, new FileSystemReportableToUrl)

}

class FileReportCreator(project: Project, title: String, reportableToUrl: FileSystemReportableToUrl) extends ReportCreator[FileSystemReportableToUrl](project, title, reportableToUrl) {
  protected def print(path: ReportableList, html: String) {
    val file = reportableToUrl.file(path)
    println(file)
    Files.printToFile(file)((p) => p.append(html))
  }
  def create {
    report.walkWithPath((path) => {
      htmlFor(path) match {
        case Some(html) => print(path, html)
        case _ => ;
      }
    })
  }
}

class ReportCreator[RtoUrl <: ReportableToUrl](project: RequirementAndHolder, title: String = null, val reportableToUrl: RtoUrl = new FileSystemReportableToUrl) {
  import Reportable._
  import Renderer._
  val report = Report(if (title == null) project.titleOrDescription("Unnamed") else title, project)
  val urlMap = reportableToUrl.makeUrlMap(report)
  val rootUrl = reportableToUrl.url(List(project, report))

  protected def engine(path: ReportableList) = path.collect { case e: Engine => e }.head

  def htmlFor(path: ReportableList) = {
    val r = path.head
    if (!urlMap.contains(r))
      throw new IllegalStateException
    val optHtml = r match {
      //        case r: Report => Some(HtmlRenderer.reportHtml(rootUrl).render(reportableToUrl, urlMap, r))
      case p: Project =>
        Some(HtmlRenderer.projectHtml(rootUrl).render(reportableToUrl, urlMap, Report("Project: " + p.titleOrDescription(ReportCreator.unnamed), p)))
      case e: Engine => Some(HtmlRenderer.engineHtml(rootUrl).render(reportableToUrl, urlMap, Report("Engine: " + e.titleOrDescription(ReportCreator.unnamed), engine(path))))
      case u: RequirementAndHolder => Some(HtmlRenderer.usecaseHtml(rootUrl, restrict = path.toSet ++ u.children).render(reportableToUrl, urlMap, Report("Usecase: " + u.titleOrDescription(ReportCreator.unnamed), engine(path))))
      case t: Test => Some(HtmlRenderer.scenarioHtml(rootUrl, t, path.toSet).render(reportableToUrl, urlMap, Report("Scenario: " + t.titleOrDescription(ReportCreator.unnamed), engine(path))))
      case _ => None
    }
    optHtml
  }

}
trait ReportableToUrl {
  import Reportable._
  protected var reqId = 0
  protected var cache = Map[Reportable, String]()
  protected var seen = Set[String]()

  /** Will return a human readable name for the reportable. Will allways return the same name for the reportable */
  def apply(r: Reportable): String = {
    val existing = cache.get(r)
    existing match {
      case Some(s) => s;
      case _ => {
        def makeNewName: String = {
          reqId += 1; val default = r.templateName + reqId;
          val result = Strings.urlClean(r match {
            case req: Requirement => { val result = req.titleOrDescription(default); if (result.length > 20) default else result }
            case _ => default;
          }).replace(" ", "_")
          if (seen.contains(result)) default else result
        }
        var result: String = null
        do {
          result = makeNewName
        } while (seen.contains(result))
        cache += (r -> result)
        seen += result
        result
      }
    }
  }

  /** Will return a human readable name for each reportable in the reversed list. Typically this is used to make a path */
  def apply(path: ReportableList, separator: String = "/"): String = path.reverse.map(apply(_)).mkString(separator)

  def urlId(r: Reportable, suffix: Option[String] = None): String = r.templateName + "_" + apply(r) + suffix.collect { case s => "_" + s }.getOrElse("")
  def url(path: ReportableList): Option[String]

  def makeUrlMap(r: ReportableHolder): UrlMap =
    r.foldWithPath(List(), UrlMap(Map(), Map()), ((acc: UrlMap, path) => {
      val u = url(path);
      if (u.isDefined) acc + (path -> u.get) else acc
    }))
}

class FileSystemReportableToUrl(val dir: File = CddRunner.directory) extends ReportableToUrl {
  import Reportable._
  def file(path: ReportableList) = new File(dir, apply(path, "\\") + "." + path.head.templateName + ".html")
  def url(path: ReportableList) = Some("file:///" + file(path).getAbsolutePath())
}

class SimpleReportableToUrl extends ReportableToUrl {
  def url(path: ReportableList) = Some("/" + apply(path) + "." + path.head.templateName + ".html")
}
class NoReportableToUrl extends ReportableToUrl {
  import Reportable._
  def dir: File = CddRunner.directory
  def url(path: ReportableList) = None
  override def hashCode = 0
  override def equals(other: Any) = other != null && other.isInstanceOf[NoReportableToUrl]
}
case class ReportableRenderer(restrict: Set[Reportable], configurers: List[RenderAttributeConfigurer] = List(), templates: Map[String, Renderer] = Map()) {
  import Reportable._
  import Renderer._

  def render(reportableToUrl: ReportableToUrl, urlMap: UrlMap, holder: ReportableHolder): String = {
    if (!restrict.isEmpty && !restrict.contains(holder)) {
      val newRenderer = copy(restrict = restrict + holder)
      return newRenderer.render(reportableToUrl, urlMap, holder)
    }

    val result = holder.foldWithPath(List(), "",
      startFn = (acc: String, path: ReportableList) =>
        acc + render(reportableToUrl, urlMap, path, path.head.templateName + "_start"),
      childFn = (acc: String, path: ReportableList) =>
        acc + render(reportableToUrl, urlMap, path, path.head.templateName),
      endFn = (acc: String, path: ReportableList) =>
        acc + render(reportableToUrl, urlMap, path, path.head.templateName + "_end"))
    result
  }

  def configureAttribute(rc: RenderAttributeConfigurer*) = copy(configurers = rc.toList ::: configurers)

  def configureReportable(templateName: String, renderer: Renderer): ReportableRenderer = copy(templates = templates + (templateName -> renderer))

  def configureReportable(templateNameAndTemplates: StringRenderer*): ReportableRenderer =
    templateNameAndTemplates.foldLeft(this)((renderer, tAndN) => renderer.configureReportable(tAndN._1, tAndN._2))

  def configureReportableHolder(templateName: String, rendererStart: Renderer, rendererEnd: Renderer): ReportableRenderer =
    copy(templates = (templates +
      (templateName + "_start" -> rendererStart) +
      (templateName + "_end" -> rendererEnd)))

  def configureReportableHolder(templateNameAndTemplates: StringRendererRenderer*): ReportableRenderer =
    templateNameAndTemplates.foldLeft(this)((renderer, tAndN) => renderer.configureReportableHolder(tAndN._1, tAndN._2, tAndN._3))

  protected def render(reportableToUrl: ReportableToUrl, urlMap: UrlMap, path: List[Reportable], templateName: String): String = {
    if (!restrict.isEmpty && !restrict.contains(path.head))
      return ""
    val optRenderer = templates.get(templateName)
    val result = optRenderer match {
      case Some(renderer: StringTemplateRenderer) =>
        renderer.clear
        for (c <- configurers)
          c.update(reportableToUrl, urlMap, path, renderer.stringTemplate)
        renderer.render(reportableToUrl, urlMap, path)
      case Some(renderer: Renderer) =>
        renderer.clear
        renderer.render(reportableToUrl, urlMap, path)
      case None => ""
    }
    result
  }
}

object Renderer {
  import Reportable._
  type StringRenderer = Tuple2[String, Renderer]
  type StringRendererRenderer = Tuple3[String, Renderer, Renderer]
  implicit def apply(s: String) = StringTemplateRenderer(s)

  val renderer = new ValueForRenderer
  val refRenderer = new ReferenceRenderer
  private val dateFormat: String = "HH:mm EEE MMM d yyyy"
  val dateFormatter = DateTimeFormat.forPattern(dateFormat);
  def base(restrict: ReportableSet): ReportableRenderer = new ReportableRenderer(restrict)
  def apply(rootUrl: Option[String], restrict: ReportableSet) = base(restrict).configureAttribute(basic(rootUrl), engineConfig, reportConfig, testConfig)

  protected def basic(rootUrl: Option[String]) = RenderAttributeConfigurer((repToUrl, urlMap, path, stringTemplate) => {
    val r = path.head
    stringTemplate.setAttribute("rootUrl", rootUrl.getOrElse(null))
    stringTemplate.setAttribute("indent", Integer.toString(path.size))
    r match {
      case req: Requirement =>
        stringTemplate.setAttribute("description", req.description.collect { case d => ValueForRender(d) }.getOrElse(null))
        stringTemplate.setAttribute("title", ValueForRender(req.titleString))
        for (ref <- req.references)
          stringTemplate.setAttribute("references", ref)
      case _ =>
    }
    r match {
      case holder: ReportableHolder =>
        stringTemplate.setAttribute("childrenCount", holder.children.size)
      case _ => ;
    }
    if (urlMap.contains(r)) {
      val url = urlMap(r)
      stringTemplate.setAttribute("url", url)
      stringTemplate.setAttribute("urlId", repToUrl.urlId(path.head))
    }
  })
  protected def engineConfig = RenderAttributeConfigurer[Engine]("Engine", (_, _, path, e, stringTemplate) => stringTemplate.setAttribute("decisionTreeNodes", e.decisionTreeNodes))
  def decisionTreeConfig(test: Option[Test]) =
    RenderAttributeConfigurer[Engine]("Engine", (reportableToUrl, urlMap, path, e, stringTemplate) =>
      stringTemplate.setAttribute("decisionTree", e.toStringWith(test match {
        case Some(t) => new HtmlWithTestIfThenPrinter(t, reportableToUrl, urlMap)
        case _ => new HtmlIfThenPrinter(reportableToUrl, urlMap)
      })))

  def setAttribute(templateName: String, attributeName: String, value: Any) =
    RenderAttributeConfigurer[Engine](templateName, (reportableToUrl, urlMap, path, e, stringTemplate) =>
      stringTemplate.setAttribute(attributeName, value))

  protected def reportConfig = RenderAttributeConfigurer[Report]("Report", (reportableToUrl, urlMap, path, r, stringTemplate) => {
    stringTemplate.setAttribute("reportDate", dateFormatter.print(System.currentTimeMillis()))
    stringTemplate.setAttribute("title", r.reportTitle)
  })

  protected def testConfig = RenderAttributeConfigurer[Test]("Scenario", (reportableToUrl, urlMap, path, t, stringTemplate) => {
    stringTemplate.setAttribute("code", ValueForRender(t.optCode.collect { case c => c.pretty } getOrElse (null)))
    stringTemplate.setAttribute("expected", ValueForRender(t.expected.getOrElse("")))
    stringTemplate.setAttribute("paramCount", t.params.size)
    stringTemplate.setAttribute("because", ValueForRender(t.because.collect { case c => c.pretty } getOrElse (null)))
    for (p <- t.params)
      p match {
        case h: HtmlDisplay =>
          stringTemplate.setAttribute("params", ValueForRender(h))
        case _ =>
          stringTemplate.setAttribute("params", ValueForRender(t.paramPrinter(p)))
      }
  })

}

trait Renderer {
  def clear;

  def render(reportableToUrl: ReportableToUrl, urlMap: UrlMap, path: List[Reportable]): String
}

object RenderAttributeConfigurer {
  import Reportable._
  def apply(fn: (ReportableToUrl, UrlMap, ReportableList, StringTemplate) => Unit) = BaseRenderAttributeConfigurer(fn)
  def apply[R <: Reportable](templateName: String, fn: (ReportableToUrl, UrlMap, ReportableList, R, StringTemplate) => Unit) = TypedRenderAttributeConfigurer[R](templateName, fn)

  case class BaseRenderAttributeConfigurer(val fn: (ReportableToUrl, UrlMap, List[Reportable], StringTemplate) => Unit) extends RenderAttributeConfigurer {
    import Reportable._
    def update(reportableToUrl: ReportableToUrl, urlMap: UrlMap, path: ReportableList, template: StringTemplate) {
      fn(reportableToUrl, urlMap, path, template)
    }
  }

  case class TypedRenderAttributeConfigurer[R <: Reportable](val templateName: String, setAttributes: (ReportableToUrl, UrlMap, List[Reportable], R, StringTemplate) => Unit) extends RenderAttributeConfigurer {
    import Reportable._
    def update(reportableToUrl: ReportableToUrl, urlMap: UrlMap, path: ReportableList, template: StringTemplate) {
      val r = path.head
      if (r.templateName == templateName)
        setAttributes(reportableToUrl, urlMap, path, r.asInstanceOf[R], template)
    }
  }
}

trait RenderAttributeConfigurer {
  import Reportable._
  def update(reportableToUrl: ReportableToUrl, urlMap: UrlMap, path: ReportableList, template: StringTemplate)
}
case class StringTemplateRenderer(template: String) extends Renderer {
  import Renderer._
  import Reportable._
  val stringTemplate = new StringTemplate(template)
  stringTemplate.registerRenderer(classOf[ValueForRender], renderer)
  stringTemplate.registerRenderer(classOf[Reference], refRenderer)

  def clear = stringTemplate.reset()

  def render(reportableToUrl: ReportableToUrl, urlMap: UrlMap, path: ReportableList): String = {
    val result = stringTemplate.toString
    result
  }
}

class ValueForRender(val value: Any) {
  override def toString = if (value == null) "" else value.toString
}

object ValueForRender {
  def apply(o: Object) = if (o == null) null else new ValueForRender(o)
}

class ValueForRenderer extends AttributeRenderer {
  def toString(o: Object): String = toString(o, "")
  def toString(o: Object, format: String): String = {
    o match {
      case v: ValueForRender => v.value match {
        case null => null
        case x: HtmlDisplay => x.htmlDisplay
        case _ => Strings.htmlEscape(o.toString())
      }
    }
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

object HtmlRenderer {
  import Reportable._
  import Renderer._

  protected val title = "$title$"
  protected val description = "$if(description)$$description$$endif$"
  protected val date = "$if(reportDate)$<hr /><div class='dateTitle'>$reportDate$</div><hr /><div>$reportDate$</div>$endif$"
  protected def titleAndDescription(clazz: String, titlePattern: String) = s"<div class='$clazz'>" + a(MessageFormat.format(titlePattern, title)) + description + "</div>"
  def a(body: String) = "$if(url)$<a $if(urlId)$id='$urlId$' $endif$href='$url$'>$endif$" + body + "$if(url)$</a>$endif$"

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

  def liveEngineHtml(rootUrl: Option[String], test: Option[Test] = None, restrict: ReportableSet = Set(), engineForm: String) = Renderer(rootUrl, restrict).
    configureAttribute(Renderer.decisionTreeConfig(test), Renderer.setAttribute("Engine", "engineForm", engineForm)).
    configureReportableHolder(reportTemplate, projectTemplate, liveEngineTemplate)

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
      "<div class='cddLogo'>$if(rootUrl)$<a id='cddLogo' href='$rootUrl$'>$endif$" + cddLogoImg + "$if(rootUrl)$</a>$endif$</div>\n" +
      "<div class='cddBox'>" + Files.getFromClassPath(getClass, "OurAdvert.xml") + "</div>\n" +
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
  val liveEngineTemplate: StringRendererRenderer =
    ("Engine", "<div class='engine'>" +
      "<div class='engineSummary'>" + titleAndDescription("engineText", "Engine {0}") + table("engineTable", refsRow, useCasesRow, nodesCountRow) + "$engineForm$",

      "</div><!-- engineSummary -->" +
      "<div class='decisionTree'>\n$decisionTree$</div><!-- decisionTree -->\n" +
      "</div><!-- engine -->\n")

  val useCaseTemplate: StringRendererRenderer =
    ("UseCase",
      "<div class='usecase'>" + "<h4>" + a(title) + "</h4>\n$if(description)$<p>$description$</p>$endif$" + "\n" + table("usecaseTable", refsRow) + "\n",
      "</div><!-- useCase -->\n")

  val useCaseWithScenariosSummarisedTemplate: StringRendererRenderer =
    ("UseCase",
      s"<div class='usecaseSummary'><h4>${a(title)}\n",
      "</h4>$if(description)$<p>$description$</p>$endif$" + "</div><!-- usecaseSummary -->\n")

  val scenarioTemplate: StringRenderer = ("Scenario", "<div class='scenario'>" + titleAndDescription("scenarioText", "Scenario: {0}") +
    table("scenarioTable",
      refsRow,
      paramsRow,
      expectedRow,
      codeRow,
      becauseRow) + "</div><!-- scenario -->\n")

  val scenarioSummaryTemplate: StringRenderer = ("Scenario", a("<img src='" + HtmlForIfThenPrinter.normalScenarioIcon + "' />"))

  def table(clazz: String, rows: String*) = {
    val result = s"<table class='$clazz'>${rows.mkString("")}</table>"
    result
  }
}


 




