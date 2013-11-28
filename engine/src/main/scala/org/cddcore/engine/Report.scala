package org.cddcore.engine

import java.text.MessageFormat

import scala.language.implicitConversions
import org.antlr.stringtemplate.AttributeRenderer
import org.antlr.stringtemplate.StringTemplate
import org.joda.time.format.DateTimeFormat
import Reportable.ReportableList
import Reportable.ReportableSet
import org.cddcore.engine.tests.CddRunner
import java.io.File

object ReportWalker {
  def childWalker = new ChildReportWalker
  def engineConclusionWalker = new EngineConclusionWalker
}
trait ReportWalker {
  import Reportable._
  /**
   * The head item in the initial path is 'walked down' and as more reportables are found start/end or child are called
   *  The route taken by by the ReportFolder may vary. For example it may walk down the engine/scenario/usecase route, or it may do engine/decision/conclusion... etc
   */
  def foldWithPath[Acc](initialPath: ReportableList, initial: Acc,
    startFn: (Acc, ReportableList) => Acc,
    childFn: (Acc, ReportableList) => Acc,
    endFn: (Acc, ReportableList) => Acc): Acc

}

class ChildReportWalker extends ReportWalker {
  import Reportable._
  def foldWithPath[Acc](path: ReportableList, initial: Acc,
    startFn: (Acc, ReportableList) => Acc,
    childFn: (Acc, ReportableList) => Acc,
    endFn: (Acc, ReportableList) => Acc): Acc = {
    val head = path.head
    head match {
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

class EngineConclusionWalker extends ReportWalker {
  import Reportable._
  def foldWithPath[Acc](path: ReportableList, initial: Acc,
    startFn: (Acc, ReportableList) => Acc,
    childFn: (Acc, ReportableList) => Acc,
    endFn: (Acc, ReportableList) => Acc): Acc = {
    val head = path.head
    head match {
      case engine: EngineBuiltFromTests[_] =>
        var acc = startFn(initial, path)
        engine.walkDecisionsAndConclusion(engine.root, (cd: ConclusionOrDecision) => cd match {
          case c: Conclusion => acc = childFn(acc, c :: path)
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

object ReportCreator {
  def unnamed = "<Unnamed>"

  def fileSystem(r: ReportableHolder, title: String = null, live: Boolean = false, reportableToUrl: FileSystemReportableToUrl = new FileSystemReportableToUrl, optUrlMap: Option[UrlMap] = None) = new FileReportCreator(r, title, live, reportableToUrl, optUrlMap)

}

class FileReportCreator(r: ReportableHolder, title: String, live: Boolean = false, reportableToUrl: FileSystemReportableToUrl, optUrlMap: Option[UrlMap]) extends ReportCreator[FileSystemReportableToUrl](r, title, live, reportableToUrl, optUrlMap) {
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

class ReportCreator[RtoUrl <: ReportableToUrl](r: ReportableHolder, title: String, val live: Boolean = false, val reportableToUrl: RtoUrl, optUrlMap: Option[UrlMap] = None) {
  import Reportable._
  import PathUtils._
  import Renderer._
  val report = r match {
    case r: Report => r
    case r: Requirement =>
      Report(if (title == null) r.titleOrDescription("Unnamed") else title, r)
    case _ => throw new IllegalArgumentException
  }
  val urlMap = optUrlMap.getOrElse(reportableToUrl.makeUrlMap(report))
  val rootUrl = reportableToUrl.url(List(r, report).distinct)
  def htmlFor(path: ReportableList) = {
    val r = path.head
    if (!urlMap.contains(r))
      throw new IllegalStateException
    val optHtml = r match {
      //        case r: Report => Some(HtmlRenderer.reportHtml(rootUrl).render(reportableToUrl, urlMap, r))
      case p: Project =>
        Some(HtmlRenderer(live).projectHtml(rootUrl).render(reportableToUrl, urlMap, Report("Project: " + p.titleOrDescription(ReportCreator.unnamed), p)))
      case e: Engine => Some(HtmlRenderer(live).engineHtml(rootUrl).render(reportableToUrl, urlMap, Report("Engine: " + e.titleOrDescription(ReportCreator.unnamed), findEngine(path))))
      case u: RequirementAndHolder => Some(HtmlRenderer(live).usecaseHtml(rootUrl, restrict = path.toSet ++ u.children).render(reportableToUrl, urlMap, Report("Usecase: " + u.titleOrDescription(ReportCreator.unnamed), findEngine(path))))
      case t: Test =>
        val conclusion = PathUtils.findEngineWithTests(path).findConclusionFor(t.params)
        Some(HtmlRenderer(live).scenarioHtml(rootUrl, conclusion, t, path.toSet).render(reportableToUrl, urlMap, Report("Scenario: " + t.titleOrDescription(ReportCreator.unnamed), findEngine(path))))
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
            case report: Report => { val result = report.title.getOrElse(default); if (result.length > 20) default else result }
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

  /** We give each reportable a unique id, so that if it occurs once in an html document, we can reference it by id */
  def urlId(r: Reportable, suffix: Option[String] = None): String = r.templateName + "_" + apply(r) + suffix.collect { case s => "_" + s }.getOrElse("")

  def url(path: ReportableList): Option[String]

  def makeUrlMap(r: ReportableHolder): UrlMap =
    r.foldWithPath(List(), UrlMap(Map(), Map()), ((acc: UrlMap, path) => {
      val u = url(path);
      if (u.isDefined) acc + (path -> u.get) else acc
    }))

  def makeUrlMapWithDecisionsAndConclusions(r: ReportableHolder): UrlMap =
    r.foldWithPath(List(), UrlMap(Map(), Map()), ((acc: UrlMap, path) => {
      def addToMap(acc: UrlMap, path: ReportableList) = {
        val u = url(path);
        val withU = if (u.isDefined) acc + (path -> u.get) else acc
        withU
      }
      val withU = addToMap(acc, path)
      path.head match {
        case e: EngineBuiltFromTests[_] => e.fold(withU, new DecisionTreeFolder[UrlMap] {
          def apply(acc: UrlMap, c: Conclusion) = addToMap(acc, c :: path)
          def apply(acc: UrlMap, d: Decision) = addToMap(acc, d :: path)
        })
        case _ => withU
      }
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
case class ReportableRenderer(restrict: Set[Reportable], configurers: List[RenderAttributeConfigurer] = List(), templates: Map[String, Renderer] = Map(), walker: ReportWalker) {
  import Reportable._
  import Renderer._

  def render(reportableToUrl: ReportableToUrl, urlMap: UrlMap, holder: ReportableHolder): String = {
    if (!restrict.isEmpty && !restrict.contains(holder)) {
      val newRenderer = copy(restrict = restrict + holder)
      return newRenderer.render(reportableToUrl, urlMap, holder)
    }

    val result = walker.foldWithPath(List(holder), "",
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
  def base(restrict: ReportableSet, walker: ReportWalker = ReportWalker.childWalker): ReportableRenderer = new ReportableRenderer(restrict, walker = walker)
  def apply(rootUrl: Option[String], restrict: ReportableSet, live: Boolean, walker: ReportWalker = ReportWalker.childWalker) = base(restrict, walker).configureAttribute(basic(rootUrl, live), engineConfig, reportConfig, testConfig)

  protected def basic(rootUrl: Option[String], live: Boolean) = RenderAttributeConfigurer((repToUrl, urlMap, path, stringTemplate) => {
    val r = path.head
    stringTemplate.setAttribute("rootUrl", rootUrl.getOrElse(null))
    stringTemplate.setAttribute("indent", Integer.toString(path.size))
    if (live)
      stringTemplate.setAttribute("live", Integer.toString(path.size))
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

  def decisionTreeConfig(params: Option[List[Any]], conclusion: Option[Conclusion], test: Option[Test]) =
    RenderAttributeConfigurer[EngineBuiltFromTests [_]]("Engine", (reportableToUrl, urlMap, path, e, stringTemplate) =>
      stringTemplate.setAttribute("decisionTree", e.toStringWith(params match {
        case Some(p) => new HtmlWithTestIfThenPrinter(p, conclusion, test, reportableToUrl, urlMap)
        case _ => new HtmlIfThenPrinter(reportableToUrl, urlMap)
      })))

  def setAttribute(templateName: String, attributeName: String, value: Any) =
    RenderAttributeConfigurer[EngineFull[_,_]](templateName, (reportableToUrl, urlMap, path, e, stringTemplate) =>
      stringTemplate.setAttribute(attributeName, value))

  protected def reportConfig = RenderAttributeConfigurer[Report]("Report", (reportableToUrl, urlMap, path, r, stringTemplate) => {
    stringTemplate.setAttribute("reportDate", dateFormatter.print(System.currentTimeMillis()))
    stringTemplate.setAttribute("title", r.reportTitle)
  })

  def addParams(st: StringTemplate, attributeName: String, paramPrinter: LoggerDisplayProcessor, params: List[Any]) {
    for (p <- params)
      p match {
        case h: HtmlDisplay =>
          st.setAttribute(attributeName, ValueForRender(h))
        case _ =>
          st.setAttribute(attributeName, ValueForRender(paramPrinter(p)))
      }
  }

  protected def testConfig = RenderAttributeConfigurer[Test]("Scenario", (reportableToUrl, urlMap, path, t, stringTemplate) => {
    stringTemplate.setAttribute("code", ValueForRender(t.optCode.collect { case c => c.pretty } getOrElse (null)))
    stringTemplate.setAttribute("expected", ValueForRender(t.expected.getOrElse("")))
    stringTemplate.setAttribute("paramCount", t.params.size)
    stringTemplate.setAttribute("because", ValueForRender(t.because.collect { case c => c.pretty } getOrElse (null)))
    addParams(stringTemplate, "params", t.paramPrinter, t.params)
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

  def apply(live: Boolean = false) = new HtmlRenderer(live)
  protected val title = "$title$"
  protected val description = "$if(description)$$description$$endif$"
  protected val date = "$if(reportDate)$<hr /><div class='dateTitle'>$reportDate$</div><hr /><div>$reportDate$</div>$endif$"
  def titleAndDescription(clazz: String, titlePattern: String) = s"<div class='$clazz'>" + a(MessageFormat.format(titlePattern, title)) + description + "</div>"
  def a(body: String) = "$if(url)$<a $if(urlId)$id='$urlId$' $endif$href='$url$'>$endif$" + body + "$if(url)$</a>$endif$"
  def aForLive = "$if(url)$<a href='$url$/live'>$endif$Live$if(url)$</a>$endif$"

  protected def cddLogoImg = "<img src='http://img24.imageshack.us/img24/4325/gp9j.png'  alt='CDD'/>"

  val expectedRow = "<tr><td class='title'>Expected</td><td class='value'>$if(expected)$$expected$$endif$</td></tr>"
  protected val codeRow = "$if(code)$<tr><td class='title'>Code</td><td class='value'>$code$</td></tr>$endif$"
  protected val becauseRow = "$if(because)$<tr><td class='title'>Because</td><td class='value'>$because$</td></tr>$endif$"
  protected val nodesCountRow = "<tr><td class='title'>Nodes</td><td class='value'>$decisionTreeNodes$</td></tr>"
  val paramsRow = "<tr><td class='title'>Parameter</td><td class='value'>$params: {p|$p$}; separator=\"<hr /> \"$</td></tr>"
  protected val useCasesRow = "$if(childrenCount)$<tr><td class='title'>Usecases</td><td class='value'>$childrenCount$</td></tr>$endif$"
  protected val scenariosRow = "$if(childrenCount)$<tr><td class='title'>Scenarios</td><td class='value'>$childrenCount$</td></tr>$endif$"
  protected val refsRow = "$if(references)$<tr><td class='title'>References</td><td class='value'>$references: {r|$r$}; separator=\", \"$</td></tr>$endif$"

  lazy val css = Files.getFromClassPath(getClass, "cdd.css")
  val reportTemplate: StringRendererRenderer = ("Report", {
    "<!DOCTYPE html><html><head><title>CDD Report: $title$</title><style>" +
      css + "\n</style></head>\n" +
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
      "<div class='engineSummary'>" + titleAndDescription("engineText", "Engine {0}") + "$if(live)$" + aForLive + "$endif$" + table("engineTable", refsRow, useCasesRow, nodesCountRow),

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

class HtmlRenderer(live: Boolean) {
  import HtmlRenderer._
  def projectHtml(rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, live).
    configureAttribute(Renderer.decisionTreeConfig(None, None, None)).
    configureReportableHolder(reportTemplate, engineTemplate, useCaseWithScenariosSummarisedTemplate).
    configureReportable(scenarioSummaryTemplate)

  def engineHtml(rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, live).
    configureAttribute(Renderer.decisionTreeConfig(None, None, None)).
    configureReportableHolder(reportTemplate, projectTemplate, engineTemplate, useCaseWithScenariosSummarisedTemplate).
    configureReportable(scenarioSummaryTemplate)

  def liveEngineHtml(rootUrl: Option[String], params: Option[List[Any]], conclusion: Option[Conclusion], restrict: ReportableSet = Set(), engineForm: String) = Renderer(rootUrl, restrict, live).
    configureAttribute(Renderer.decisionTreeConfig(params, conclusion, None), Renderer.setAttribute("Engine", "engineForm", engineForm), Renderer.setAttribute("Engine", "live", true)).
    configureReportableHolder(reportTemplate, projectTemplate, liveEngineTemplate)

  def usecaseHtml(rootUrl: Option[String], test: Option[Test] = None, restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, live).
    configureAttribute(Renderer.decisionTreeConfig(None, None, test)).
    configureReportableHolder(reportTemplate, projectTemplate, engineTemplate, useCaseTemplate).
    configureReportable(scenarioTemplate)

  def scenarioHtml(rootUrl: Option[String], conclusion: Conclusion, test: Test, restrict: ReportableSet = Set()) = Renderer(rootUrl, restrict, live).
    configureAttribute(Renderer.decisionTreeConfig(Some(test.params), Some(conclusion), Some(test))).
    configureReportableHolder(reportTemplate, projectTemplate, engineTemplate, useCaseTemplate).
    configureReportable(scenarioTemplate)
}
 




