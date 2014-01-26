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
import javax.rmi.CORBA.GetORBPropertiesFileAction

/** When reporting, this keeps track of what url a reportable is associated with.  */
case class UrlMap(val toUrl: Map[Reportable, String], val fromUrl: Map[String, List[Reportable]]) {
  import Reportable._
  /** From a reportable to the Url representing it */
  def apply(r: Reportable): String = toUrl(r)
  /** From a reportable to the optional Url representing it */
  def get(r: Reportable): Option[String] = toUrl.get(r)

  /** From a url to the reportable that should be at that url */
  def apply(url: String) = fromUrl(url)
  /** From a url to the optional reportable that should be at that url */
  def get(url: String) = fromUrl.get(url)

  /** Has the reportable got a url? */
  def contains(r: Reportable) = toUrl.contains(r)
  /** Makes a new UrlMap with the path mapping to a url */
  def +(kv: (ReportableList, String)) = UrlMap(toUrl + (kv._1.head -> kv._2), fromUrl + (kv._2 -> kv._1))
  def size = toUrl.size
}

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

  protected def walkChildren[Acc](holder: ReportableHolder, path: ReportableList, initial: Acc,
    startFn: (Acc, ReportableList) => Acc,
    childFn: (Acc, ReportableList) => Acc,
    endFn: (Acc, ReportableList) => Acc) = {
    var acc = startFn(initial, path)
    for (c <- holder.children)
      acc = foldWithPath(c :: path, acc, startFn, childFn, endFn)
    acc = endFn(acc, path)
    acc
  }
}

class ChildReportWalker extends ReportWalker {
  import Reportable._
  def foldWithPath[Acc](path: ReportableList, initial: Acc,
    startFn: (Acc, ReportableList) => Acc,
    childFn: (Acc, ReportableList) => Acc,
    endFn: (Acc, ReportableList) => Acc): Acc = {
    val head = path.head
    head match {
      case holder: ReportableHolder => walkChildren(holder, path, initial, startFn, childFn, endFn)
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
      case holder: ReportableHolder => walkChildren(holder, path, initial, startFn, childFn, endFn)
      case _ => childFn(initial, path)
    }
  }

}

object ReportCreator {
  def unnamed = "<Unnamed>"

  def fileSystem(loggerDisplayProcessor: LoggerDisplayProcessor, r: ReportableHolder, title: String = null, live: Boolean = false, reportableToUrl: FileSystemReportableToUrl = new FileSystemReportableToUrl, optUrlMap: Option[UrlMap] = None) = new FileReportCreator(loggerDisplayProcessor, r, title, live, reportableToUrl, optUrlMap)

}

class FileReportCreator(loggerDisplayProcessor: LoggerDisplayProcessor, r: ReportableHolder, title: String, live: Boolean = false, reportableToUrl: FileSystemReportableToUrl, optUrlMap: Option[UrlMap]) extends ReportCreator[FileSystemReportableToUrl](loggerDisplayProcessor, r, title, live, reportableToUrl, optUrlMap) {
  protected def print(path: ReportableList, html: String) {
    val file = reportableToUrl.file(path)
    println(file)
    Files.printToFile(file)((p) => p.append(html))
  }
  protected def makeReport = (path: List[Reportable]) =>
    htmlFor(path) match {
      case Some(html) => print(path, html)
      case _ => ;
    }
  def create = {
    report.walkWithPath(makeReport)
    val documents = Reportable.documentsIn(report)
    documents.map(List(_, report)).foreach(makeReport)
  }
}

class ReportCreator[RtoUrl <: ReportableToUrl](loggerDisplayProcessor: LoggerDisplayProcessor, r: ReportableHolder, title: String, val live: Boolean = false, val reportableToUrl: RtoUrl, optUrlMap: Option[UrlMap] = None) {
  import Reportable._
  import PathUtils._
  import Renderer._
  val report = r match {
    case r: Report => r
    case r: Requirement =>
      Report(if (title == null) r.titleOrDescription("Unnamed") else title, r)
    case null => throw new NullPointerException("Is this because your item is a val and not a lazy val?")
    case _ => throw new IllegalArgumentException(r.toString)
  }
  val urlMap = optUrlMap.getOrElse(reportableToUrl.makeUrlMap(report))
  val rootUrl = reportableToUrl.url(List(r, report).distinct)
  def htmlFor(path: ReportableList) = {
    val pathHead = path.head
    if (!urlMap.contains(pathHead))
      throw new IllegalStateException
    val optHtml = pathHead match {
      //        case r: Report => Some(HtmlRenderer.reportHtml(rootUrl).render(reportableToUrl, urlMap, r))
      case d: Document => {
        val strategy = new ByReferenceDocumentPrinterStrategy(Some(d), new SimpleKeyStrategy)
        val documentDetails = strategy.makeReportOfJustDocuments(r)
        val report = Report(d.titleString, documentDetails)
        val renderer = HtmlRenderer(loggerDisplayProcessor, true).documentsHtml(rootUrl)
        val html = renderer.render(reportableToUrl, urlMap, report)
        Some(html)
      }
      case p: Project =>
        Some(HtmlRenderer(loggerDisplayProcessor, live).projectHtml(rootUrl).render(reportableToUrl, urlMap, Report("Project: " + p.titleOrDescription(ReportCreator.unnamed), p)))
      case e: Engine => Some(HtmlRenderer(loggerDisplayProcessor, live).engineHtml(rootUrl).render(reportableToUrl, urlMap, Report("Engine: " + e.titleOrDescription(ReportCreator.unnamed), findEngine(path))))
      case u: UseCase => Some(HtmlRenderer(loggerDisplayProcessor, live).usecaseHtml(rootUrl, restrict = path.toSet ++ u.children).render(reportableToUrl, urlMap, Report("Usecase: " + u.titleOrDescription(ReportCreator.unnamed), findEngine(path))))
      case t: Test =>
        val conclusion = PathUtils.findEngineWithTests(path).findConclusionFor(t.params)
        Some(HtmlRenderer(loggerDisplayProcessor, live).scenarioHtml(rootUrl, conclusion, t, path.toSet).render(reportableToUrl, urlMap, Report("Scenario: " + t.titleString, findEngine(path))))
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

  protected def findAndAddToCacheIfNeed(r: Reportable): String = {
    val existing = cache.get(r)
    existing match {
      case Some(s) => s;
      case _ => {
        def makeNewName: String = {
          reqId += 1; val default = templateName(r) + reqId;
          val result = Strings.urlClean(r match {
            case report: Report => { val result = report.title.getOrElse(default); if (result.length > 20) default else result }
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

  /** Will return a human readable name for the reportable. Will allways return the same name for the reportable */
  def apply(r: Reportable): String = {
    r match {
      case r: ReportableWrapper => findAndAddToCacheIfNeed(r.delegate.getOrElse(r))
      case _ => findAndAddToCacheIfNeed(r)
    }
  }

  /** Will return a human readable name for each reportable in the reversed list. Typically this is used to make a path */
  def apply(path: ReportableList, separator: String = "/"): String = path.reverse.map(apply(_)).mkString(separator)

  /** We give each reportable a unique id, so that if it occurs once in an html document, we can reference it by id */
  def urlId(r: Reportable, suffix: Option[String] = None): String = templateName(r) + "_" + apply(r) + suffix.collect { case s => "_" + s }.getOrElse("")

  def url(path: ReportableList): Option[String]

  protected def add = (urlMap: UrlMap, path: ReportableList) => {
    val u = url(path);
    if (u.isDefined) urlMap + (path -> u.get) else urlMap
  }

  def makeUrlMap(r: ReportableHolder): UrlMap = {
    val fromBasicReportables = r.foldWithPath(UrlMap(Map(), Map()), add)
    documentsIn(r).map(List(_, r)).foldLeft(fromBasicReportables)(add)
  }

  def makeUrlMapWithDecisionsAndConclusions(r: ReportableHolder): UrlMap =
    r.foldWithPath(UrlMap(Map(), Map()), ((acc: UrlMap, path) => {
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
  def file(path: ReportableList) = new File(dir, apply(path, "\\") + "." + templateName(path) + ".html")
  def url(path: ReportableList) = Some("file:///" + file(path).getAbsolutePath())
}

class SimpleReportableToUrl extends ReportableToUrl {
  def url(path: ReportableList) = Some("/" + apply(path) + "." + Reportable.templateName(path) + ".html")
}
class NoReportableToUrl extends ReportableToUrl {
  import Reportable._
  def dir: File = CddRunner.directory
  def url(path: ReportableList) = None
  override def hashCode = 0
  override def equals(other: Any) = other != null && other.isInstanceOf[NoReportableToUrl]
}
case class ReportableRenderer(loggerDisplayProcessor: LoggerDisplayProcessor, restrict: Set[Reportable], configurers: List[RenderAttributeConfigurer] = List(), templates: Map[String, Renderer] = Map(), walker: ReportWalker, rootUrl: Option[String]) {
  import Reportable._
  import Renderer._

  def render(reportableToUrl: ReportableToUrl, urlMap: UrlMap, holder: ReportableHolder): String = {
    if (!restrict.isEmpty && !restrict.contains(holder)) {
      val newRenderer = copy(restrict = restrict + holder)
      return newRenderer.render(reportableToUrl, urlMap, holder)
    }

    def addIt(postFix: String) = (acc: String, path: ReportableList) => {
      val t = templateName(path) + postFix;
      val result = render(reportableToUrl, loggerDisplayProcessor, urlMap, path, t)
      acc + result
    }
    val result = walker.foldWithPath(List(holder), "", addIt("_start"), addIt(""), addIt("_end"))
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

  protected def render(reportableToUrl: ReportableToUrl, loggerDisplayProcessor: LoggerDisplayProcessor, urlMap: UrlMap, path: List[Reportable], templateName: String): String = {
    if (!restrict.isEmpty && !restrict.contains(path.head))
      return ""
    val optRenderer = templates.get(templateName)
    val result = optRenderer match {
      case Some(renderer: StringTemplateRenderer) =>
        renderer.clear
        for (c <- configurers)
          c.update(reportableToUrl, loggerDisplayProcessor, urlMap, path, renderer.stringTemplate, rootUrl)
        renderer.render(reportableToUrl, urlMap, path)
      case Some(renderer: Renderer) =>
        renderer.clear
        renderer.render(reportableToUrl, urlMap, path)
      case None => ""
    }
    result
  }
}

case class RendererContext[R <: Reportable](reportableToUrl: ReportableToUrl, loggerDisplayProcessor: LoggerDisplayProcessor, urlMap: UrlMap, path: List[Reportable], r: R, stringTemplate: StringTemplate, rootUrl: Option[String])

object Renderer {
  import Reportable._
  type StringRenderer = Tuple2[String, Renderer]
  type StringRendererRenderer = Tuple3[String, Renderer, Renderer]
  implicit def apply(s: String) = StringTemplateRenderer(s)

  val engineFromTestsKey = "EngineFromTests"
  val engineChildKey = "EngineChild"
  val engineWithChildrenKey = "EngineWithChildren"
  val mergedReportableKey = "Merged"

  val renderer = new ValueForRenderer
  val refRenderer = new ReferenceRenderer
  private val dateFormat: String = "HH:mm EEE MMM d yyyy"
  val dateFormatter = DateTimeFormat.forPattern(dateFormat);
  def base(loggerDisplayProcessor: LoggerDisplayProcessor, rootUrl: Option[String], restrict: ReportableSet, walker: ReportWalker = ReportWalker.childWalker): ReportableRenderer =
    new ReportableRenderer(loggerDisplayProcessor, restrict, walker = walker, rootUrl = rootUrl)
  def apply(loggerDisplayProcessor: LoggerDisplayProcessor, rootUrl: Option[String], restrict: ReportableSet, live: Boolean, walker: ReportWalker = ReportWalker.childWalker) =
    base(loggerDisplayProcessor, rootUrl, restrict, walker).configureAttribute(basic(live), engineConfig, reportConfig, testConfig)

  protected def basic(live: Boolean) = RenderAttributeConfigurer((rendererContext) => {
    import rendererContext._
    val r = path.head
    stringTemplate.setAttribute("rootUrl", rootUrl.getOrElse(null))
    stringTemplate.setAttribute("indent", Integer.toString(path.size))
    if (live)
      stringTemplate.setAttribute("live", Integer.toString(path.size))
    def addFromRequirement(r: Requirement) {
      stringTemplate.setAttribute("description", r.description.collect { case d => ValueForRender(d) }.getOrElse(null))
      stringTemplate.setAttribute("title", ValueForRender(r.titleString))
      for (ref <- r.references)
        stringTemplate.setAttribute("references", ref)
      if (urlMap.contains(r)) {
        val url = urlMap(r)
        stringTemplate.setAttribute("url", url)
        stringTemplate.setAttribute("urlId", reportableToUrl.urlId(path.head))
      }
    }
    r match {
      case r: MergedReportable =>
        stringTemplate.setAttribute("key", ValueForRender(r.key))
      case t: MergedTitle =>
        stringTemplate.setAttribute("title", ValueForRender(t.title.getOrElse("")))
      case d: MergedDescription =>
        stringTemplate.setAttribute("description", ValueForRender(d.description.getOrElse("")))
      case m: MergedShortDescription =>
        stringTemplate.setAttribute("title", ValueForRender(m.name))
      case re: RequirementAndEngine =>
        addFromRequirement(re.reportable)
        val engineName = re.engine.collect { case e: Engine => e.titleOrDescription("") }.getOrElse("")
        stringTemplate.setAttribute("engineName", ValueForRender(engineName))
        stringTemplate.setAttribute("engineSummary", ValueForRender(Strings.firstCharacters(engineName)))
      case req: Requirement => addFromRequirement(req)

      case _ =>
    }
    r match {
      case holder: ReportableHolder =>
        stringTemplate.setAttribute("childrenCount", holder.children.size)
      case _ => ;
    }
  })

  protected def engineConfig = RenderAttributeConfigurer[Engine](Set(engineFromTestsKey), (rc) => { import rc._; stringTemplate.setAttribute("decisionTreeNodes", r.decisionTreeNodes) })

  def decisionTreeConfig(params: Option[List[Any]], conclusion: Option[Conclusion], test: Option[Test]) =
    {
      val fn = (rc: RendererContext[EngineBuiltFromTests[_]]) => {
        import rc._
        stringTemplate.setAttribute("decisionTree", r.toStringWith(params match {
          case Some(p) => new HtmlWithTestIfThenPrinter(p, conclusion, test, reportableToUrl, urlMap)
          case _ => new HtmlIfThenPrinter(reportableToUrl, urlMap)
        }))
      }
      RenderAttributeConfigurer[EngineBuiltFromTests[_]](Set(engineChildKey, engineFromTestsKey), fn)
    }

  def setMergedReportable: RenderAttributeConfigurer = RenderAttributeConfigurer[MergedReportable](Set(mergedReportableKey), (rc) => {
    import rc._
    val htmlRenderer = new HtmlRenderer(rc.loggerDisplayProcessor, false)
    val renderer = htmlRenderer.mergedReportableTitle(rootUrl)
    val html = r.titles.foldLeft("")((acc, mt) =>
      acc + renderer.render(reportableToUrl, urlMap, mt))
    stringTemplate.setAttribute("mergedReportable", html)
    if (r.titles.length != 1)
      stringTemplate.setAttribute("multipleMergedValuesFlag", "true")
  })

  def setAttribute(templateName: String, attributeName: String, value: Any) =
    RenderAttributeConfigurer[EngineBuiltFromTests[_]](Set(templateName), (rc) => { import rc._; stringTemplate.setAttribute(attributeName, value) })

  protected def reportConfig = RenderAttributeConfigurer[Report](Set("Report"), (rc) => {
    import rc._
    stringTemplate.setAttribute("reportDate", dateFormatter.print(System.currentTimeMillis()))
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

  protected def testConfig = RenderAttributeConfigurer[Test](Set("Scenario"), (rendererContext) => {
    import rendererContext._
    import r._
//    stringTemplate.setAttribute("title", ValueForRender(titleString))
    stringTemplate.setAttribute("code", ValueForRender(optCode.collect { case c => c.pretty } getOrElse (null)))
    stringTemplate.setAttribute("expected", ValueForRender(expected.getOrElse("")))
    stringTemplate.setAttribute("paramCount", params.size)
    stringTemplate.setAttribute("because", ValueForRender(r.because.collect { case c => c.pretty } getOrElse (null)))
    addParams(stringTemplate, "params", loggerDisplayProcessor, params)
  })
}

trait Renderer {
  def clear;

  def render(reportableToUrl: ReportableToUrl, urlMap: UrlMap, path: List[Reportable]): String
}

object RenderAttributeConfigurer {
  import Reportable._
  def apply(fn: (RendererContext[_]) => Unit) = BaseRenderAttributeConfigurer(fn)
  def apply[R <: Reportable](templateNames: Set[String], fn: (RendererContext[R]) => Unit) = TypedRenderAttributeConfigurer[R](templateNames, fn)

  case class BaseRenderAttributeConfigurer(val fn: (RendererContext[_]) => Unit) extends RenderAttributeConfigurer {
    import Reportable._
    def update(reportableToUrl: ReportableToUrl, loggerDisplayProcessor: LoggerDisplayProcessor, urlMap: UrlMap, path: ReportableList, template: StringTemplate, rootUrl: Option[String]) {
      fn(RendererContext[Reportable](reportableToUrl, loggerDisplayProcessor, urlMap, path, path.head, template, rootUrl))
    }
  }

  case class TypedRenderAttributeConfigurer[R <: Reportable](val templateNames: Set[String], setAttributes: (RendererContext[R]) => Unit) extends RenderAttributeConfigurer {
    import Reportable._
    def update(reportableToUrl: ReportableToUrl, loggerDisplayProcessor: LoggerDisplayProcessor, urlMap: UrlMap, path: ReportableList, template: StringTemplate, rootUrl: Option[String]) {
      val r = Reportable.unwrap(path.head)
      if (templateNames.contains(Reportable.templateName(r)))
        setAttributes(RendererContext[R](reportableToUrl, loggerDisplayProcessor, urlMap, path, r.asInstanceOf[R], template, rootUrl))
    }
  }
}

trait RenderAttributeConfigurer {
  import Reportable._
  def update(reportableToUrl: ReportableToUrl, loggerDisplayProcessor: LoggerDisplayProcessor, urlMap: UrlMap, path: ReportableList, template: StringTemplate, rootUrl: Option[String])
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

  def apply(loggerDisplayProcessor: LoggerDisplayProcessor, live: Boolean = false) = new HtmlRenderer(loggerDisplayProcessor, live)
  protected val title = "$title$"
  protected val description = "$if(description)$$description$$endif$"
  protected val date = "$if(reportDate)$<hr /><div class='dateTitle'>$reportDate$</div><hr /><div>$reportDate$</div>$endif$"
  def titleAndDescription(clazz: String, titlePattern: String, iconPrefix: String = "") =
    s"<div class='$clazz'>" + a(iconPrefix + MessageFormat.format(titlePattern, title)) + description + "</div>"
  def a(body: String) = "$if(url)$<a $if(urlId)$id='$urlId$' $endif$href='$url$'>$endif$" + body + "$if(url)$</a>$endif$"
  def a(body: String, title: String) = "$if(url)$<a $if(urlId)$id='$urlId$' $endif$href='$url$' title='" + title + "'>$endif$" + body + "$if(url)$</a>$endif$"
  def aForLive = "$if(url)$<a id='$url$/live' href='$url$/live'>$endif$Live$if(url)$</a>$endif$"

  protected def cddLogo = "<img src='http://img32.imageshack.us/img32/8151/xy9u.png'  alt='Report Home Page'/>"
  protected def engineWithChildrenIcon = "<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engineFold2_zpsb62930b9.png'  alt='engine with children icon'/>"
  protected def childEngineIcon = "<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engineChild_zps3d29a414.png'  alt='child engine icon'/>"
  protected def engineWithTestsIcon = "<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png'  alt='engine with tests icon'/>"
  protected def usecaseIcon = "<img src='http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png'  alt='usecase icon'/>"

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
      "<div class='cddLogo'>$if(rootUrl)$<a id='cddLogo' href='$rootUrl$'>$endif$" + cddLogo + "$if(rootUrl)$</a>$endif$</div>\n" +
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

  val mergedTemplate: StringRendererRenderer =
    ("Merged", "<div class='merged'><b>$key$</b> $if(multipleMergedValuesFlag)$<div class='multipleTitles'>$endif$$mergedReportable$", "$if(multipleMergedValuesFlag)$</div>$endif$</div><!-- merged -->\n")

  val mergedTitleTemplate: StringRendererRenderer =
    ("MergedTitle", "<b>$title$</b>", "\n")

  val mergedDescriptionTemplate: StringRendererRenderer =
    //  ("MergedDescription", "", "$description$")
    ("MergedDescription", "", "<div class='mergedDescription'>$description$</div>\n")

  // So why is this an engine with tests icon? Well it turns out that this is displayed as part of a use case anyway, and the engine with tests followed by a summary of the name of the engine 'looks better' 
  val useCaseIconTemplate: StringRendererRenderer = ("UseCase", a(engineWithTestsIcon + "$engineSummary$", "$engineName$"), "")

  val requirementAndEngineTemplate: StringRendererRenderer =
    ("RequirementAndEngine", "<div class='requirementAndEngine'> $title$ ", "</div><!-- requirementAndEngine -->\n")

  val engineWithTestsTemplate: StringRendererRenderer =
    (engineFromTestsKey, "<div class='engineWithTests'>" +
      "<div class='engineSummary'>" + titleAndDescription("engineText", "Engine {0}", engineWithTestsIcon) + "$if(live)$" + aForLive + "$endif$" + table("engineTable", refsRow, useCasesRow, nodesCountRow),

      "</div><!-- engineSummary -->" +
      "<div class='decisionTree'>\n$decisionTree$</div><!-- decisionTree -->\n" +
      "</div><!-- engine -->\n")
  val childEngineTemplate: StringRendererRenderer =
    (engineChildKey, "<div class='childEngine'>" +
      "<div class='engineSummary'>" + titleAndDescription("engineText", "Engine {0}", childEngineIcon) + "$if(live)$" + aForLive + "$endif$" + table("engineTable", refsRow, useCasesRow, nodesCountRow),

      "</div><!-- engineSummary -->" +
      "<div class='decisionTree'>\n$decisionTree$</div><!-- decisionTree -->\n" +
      "</div><!-- engine -->\n")

  val engineWithChildEngineTemplate: StringRendererRenderer =
    (engineWithChildrenKey, "<div class='engineWithChildren'>" +
      "<div class='engineWithChildrenSummary'>" + titleAndDescription("engineText", "Engine {0}", engineWithChildrenIcon) + "$if(live)$" + aForLive + "$endif$" + table("engineTable", refsRow, useCasesRow, nodesCountRow),

      "</div><!-- engineWithChildrenSummary -->" +
      "</div><!-- engine -->\n")

  val liveEngineTemplate: StringRendererRenderer =
    (engineFromTestsKey, "<div class='engine'>" +
      "<div class='engineSummary'>" + titleAndDescription("engineText", "Engine {0}") + table("engineTable", refsRow, useCasesRow, nodesCountRow) + "$engineForm$",

      "</div><!-- engineSummary -->" +
      "<div class='decisionTree'>\n$decisionTree$</div><!-- decisionTree -->\n" +
      "</div><!-- engine -->\n")

  val useCaseTemplate: StringRendererRenderer =
    ("UseCase",
      "<div class='usecase'>" + "<h4>" + a(usecaseIcon + title) + "</h4>\n$if(description)$<p>$description$</p>$endif$" + "\n" + table("usecaseTable", refsRow) + "\n",
      "</div><!-- useCase -->\n")

  val useCaseWithScenariosSummarisedTemplate: StringRendererRenderer =
    ("UseCase",
      s"<div class='usecaseSummary'><h4>${a(usecaseIcon + title)}\n",
      "</h4>$if(description)$<p>$description$</p>$endif$" + "</div><!-- usecaseSummary -->\n")

  val scenarioTemplate: StringRenderer = ("Scenario", "<div class='scenario'>" + titleAndDescription("scenarioText", "Scenario: {0}") +
    table("scenarioTable",
      refsRow,
      paramsRow,
      expectedRow,
      codeRow,
      becauseRow) + "</div><!-- scenario -->\n")

  val scenarioSummaryTemplate: StringRenderer = ("Scenario", a("<img src='" + HtmlForIfThenPrinter.normalScenarioIcon + "' $if(title)$title='$title$'$endif$ />"))

  def table(clazz: String, rows: String*) = {
    val result = s"<table class='$clazz'>${rows.mkString("")}</table>"
    result
  }
}

class HtmlRenderer(loggerDisplayProcessor: LoggerDisplayProcessor, live: Boolean) {
  import HtmlRenderer._
  def projectHtml(rootUrl: Option[String], restrict: ReportableSet = Set()) =
    Renderer(loggerDisplayProcessor, rootUrl, restrict, live).
      configureAttribute(Renderer.decisionTreeConfig(None, None, None)).
      configureReportableHolder(reportTemplate, engineWithTestsTemplate, engineWithChildEngineTemplate, childEngineTemplate, useCaseWithScenariosSummarisedTemplate).
      configureReportable(scenarioSummaryTemplate)

  def engineHtml(rootUrl: Option[String], restrict: ReportableSet = Set()) = Renderer(loggerDisplayProcessor, rootUrl, restrict, live).
    configureAttribute(Renderer.decisionTreeConfig(None, None, None)).
    configureReportableHolder(reportTemplate, projectTemplate, engineWithTestsTemplate, engineWithChildEngineTemplate, childEngineTemplate, useCaseWithScenariosSummarisedTemplate).
    configureReportable(scenarioSummaryTemplate)

  def liveEngineHtml(rootUrl: Option[String], params: Option[List[Any]], conclusion: Option[Conclusion], restrict: ReportableSet = Set(), engineForm: String) =
    Renderer(loggerDisplayProcessor, rootUrl, restrict, live).
      configureAttribute(Renderer.decisionTreeConfig(params, conclusion, None), Renderer.setAttribute(Renderer.engineFromTestsKey, "engineForm", engineForm), Renderer.setAttribute("Engine", "live", true)).
      configureReportableHolder(reportTemplate, projectTemplate, liveEngineTemplate)

  def usecaseHtml(rootUrl: Option[String], test: Option[Test] = None, restrict: ReportableSet = Set()) =
    Renderer(loggerDisplayProcessor, rootUrl, restrict, live).
      configureAttribute(Renderer.decisionTreeConfig(None, None, test)).
      configureReportableHolder(reportTemplate, projectTemplate, engineWithTestsTemplate, engineWithChildEngineTemplate, childEngineTemplate, useCaseTemplate).
      configureReportable(scenarioTemplate)

  def scenarioHtml(rootUrl: Option[String], conclusion: Conclusion, test: Test, restrict: ReportableSet = Set()) =
    Renderer(loggerDisplayProcessor, rootUrl, restrict, live).
      configureAttribute(Renderer.decisionTreeConfig(Some(test.params), Some(conclusion), Some(test))).
      configureReportableHolder(reportTemplate, projectTemplate, engineWithTestsTemplate, engineWithChildEngineTemplate, childEngineTemplate, useCaseTemplate).
      configureReportable(scenarioTemplate)

  def documentsHtml(rootUrl: Option[String]) = Renderer(loggerDisplayProcessor, rootUrl, Set(), live).
    configureAttribute(Renderer.setMergedReportable).
    configureReportableHolder(reportTemplate, mergedTemplate)

  def mergedReportableTitle(rootUrl: Option[String]) = Renderer(loggerDisplayProcessor, rootUrl, Set(), live).
    configureReportableHolder(mergedTemplate, mergedTitleTemplate, mergedDescriptionTemplate,
      requirementAndEngineTemplate, useCaseIconTemplate).
      configureReportable(scenarioSummaryTemplate)

}

trait DocumentPrinterStrategy {
  /** The report passed in probably holds engines. The report coming out holds the structure to be printed */
  def makeReportOfJustDocuments(report: ReportableHolder): Reportable
}

class SimpleDocumentPrinterStrategy extends DocumentPrinterStrategy {
  def makeReportOfJustDocuments(report: ReportableHolder) = SimpleRequirementAndHolder(report)
}

case class RequirementAndEngine(reportable: RequirementAndHolder, engine: Option[Engine]) extends ReportableHolder with ReportableWrapper {
  def delegate = Some(reportable)
  def children = reportable.children
}

class ByReferenceDocumentPrinterStrategy(document: Option[Document], keyStrategy: KeyStrategy, debug: Boolean = false) extends DocumentPrinterStrategy {
  type ReportableToPath = Map[Reportable, List[Reportable]]
  type ReportableToKey = Map[Reportable, String]
  def findReportableToPathFor(r: ReportableHolder) = r.foldWithPath[ReportableToPath](Map(), (acc, path) => acc + (path.head -> path))

  def findReferenceFor(document: Option[Document], r: Reportable) = r match {
    case r: Requirement => r.references.find((ref) => ref.document == document)
    case _ => None
  }

  def addToFor(document: Option[Document], reportableToPath: ReportableToPath, mapToKey: ReportableToKey, r: Reportable): ReportableToKey = {
    if (mapToKey.contains(r))
      mapToKey
    else
      findReferenceFor(document, r) match {
        case Some(ref) =>
          if (debug)
            println("raw " + ref.ref + "<--" + Reportable.templateNameAndTitle(r))
          mapToKey + (r -> ref.ref)
        case _ => {
          val result = reportableToPath(r) match {
            case me :: (parent: ReportableHolder) :: tail =>
              val withParent = addToFor(document, reportableToPath, mapToKey, parent);
              withParent.get(parent) match {
                case Some(parentRef) => {
                  val myRef = keyStrategy.findKeyFor(parentRef, parent.children.reverse, me)
                  if (debug)
                    println("raw " + myRef + "<--" + Reportable.templateNameAndTitle(me))
                  withParent + (me -> myRef)
                }
                case _ => if (document.isDefined) withParent else withParent + (me -> keyStrategy.rootKey) // i.e. we are part of the document 'None'
              }
            case me :: Nil =>
              if (document.isDefined) mapToKey else mapToKey + (me -> keyStrategy.rootKey) // i.e. we are part of the document 'None'
            case _ => throw new IllegalStateException
          }
          result
        }
      }
  }

  def findReportableToRef(report: ReportableHolder, reportableToPath: ReportableToPath) = {
    val reportableToRef = report.foldLeft[ReportableToKey](Map())((acc, r) => addToFor(document, reportableToPath, acc, r))
    reportableToRef
  }

  def findStructuredMap(report: ReportableHolder) = {
    val reportableToPath = findReportableToPathFor(report)
    val reportableToRef = findReportableToRef(report, reportableToPath)
    val structuredMap = report.foldLeft(StructuredMapOfList[RequirementAndEngine]())((acc, r) => r match {
      case t: Test => acc
      case r: RequirementAndHolder =>
        {
          reportableToRef.get(r) match {
            case Some(key) => {
              val path = reportableToPath(r)
              val engine = PathUtils.findEnginePathIfExists(path)
              acc + (key -> RequirementAndEngine(r, engine))
            }
            case _ => acc
          }
        }
    })
    structuredMap
  }

  def findMergedStructuredMap(from: StructuredMapOfList[RequirementAndEngine]) = {
    val modifiedMap = from.fold[StructuredMap[Reportable]](StructuredMap())((acc, key, list) => list match {
      case Nil =>
        if (debug)
          println("placeholder: " + key)
        acc + (key -> new SimpleRequirementAndHolder(None, None, None, None, Set(), List()))
      case list =>
        if (debug)
          println("MergingX: " + key + ": " + list.map(Reportable.templateNameAndTitle(_)))
        acc + (key -> MergedReportable.makeFrom(key, list, List()))
    })
    modifiedMap
  }

  def mergedMapToReportable(from: StructuredMap[Reportable], key: Key): ReportableHolder = {
    val children = from.kidsOf(key).map(mergedMapToReportable(from, _))
    from(key.key) match {
      case m: MergedReportable => m.copy(children = children)
      case s: SimpleRequirementAndHolder => s.copy(children = children)
    }
  }

  def makeReportOfJustDocuments(report: ReportableHolder) = {
    val structuredMap = findStructuredMap(report)
    val modifiedMap = findMergedStructuredMap(structuredMap)
    val result = mergedMapToReportable(modifiedMap, "")
    result
  }
}

object SimpleRequirementAndHolder {
  def withJustChildren(children: Reportable*) = new SimpleRequirementAndHolder(None, None, None, None, Set(), children.toList)
  def apply(r: Reportable): Requirement = r match {
    case r: SimpleRequirementAndHolder => r
    case rh: RequirementAndHolder => apply(rh, rh.children.map(apply(_)))
    case r: Requirement => r
    case _ => throw new IllegalStateException(r.getClass + "\n" + r);
  }

  def apply(r: Requirement, replacementChildren: ReportableList): Requirement =
    (r, replacementChildren) match {
      case (s: Test, List()) => s
      case _ => new SimpleRequirementAndHolder(Some(r), r.title, r.description, r.priority, r.references, replacementChildren)
    }

}

case class SimpleRequirementAndHolder(delegate: Option[Reportable], title: Option[String], description: Option[String], priority: Option[Int], references: Set[Reference], children: ReportableList) extends RequirementAndHolder with ReportableWrapper {
  protected def shortToString(r: Any): String = r match {
    case m: MergedShortDescription => Reportable.templateName(m) + "(" + m.name + ")"
    case r: SimpleRequirementAndHolder => Reportable.templateName(r) + "(" + Reportable.templateName(r.delegate.getOrElse(None)) + "/" + r.delegate.collect { case r: Requirement => r.titleString } + ", children=" + r.children.map(shortToString(_)).mkString(",") + ")"
    case t: Test => Reportable.templateName(r) + "(" + t.params.mkString(",") + ")"
    case r: RequirementAndHolder => Reportable.templateName(r) + "(" + r.titleString + ", children=" + r.children.map(shortToString(_)).mkString(",") + ")"
    case r: Requirement => Reportable.templateName(r) + "(" + r.titleString + ")"
  }
  override def toString = shortToString(this)
}

class DocumentPrinter(report: Report, strategy: DocumentPrinterStrategy = new SimpleDocumentPrinterStrategy) {
  val actualReport = strategy.makeReportOfJustDocuments(report)

}

trait MergedShortDescription {
  def name: String
}
object MergedReportable {
  def makeFrom(key: String, res: List[RequirementAndEngine], children: ReportableList): MergedReportable = {
    val titles = res.groupBy(_.reportable.title).collect {
      case (title, list) =>
        val descriptions = list.groupBy(_.reportable.description).collect { case (description, list) => MergedDescription(description, list) }.toList
        MergedTitle(title, descriptions)
    }.toList
    new MergedReportable(key, titles, children)
  }
}

/** A merged requirement is used to handle the fact that many reportables may implement a '2.1' of a document. */
case class MergedReportable(val key: String, val titles: List[MergedTitle], val children: ReportableList) extends ReportableHolder with MergedShortDescription with ReportableWithTemplate {
  def name = key + "/" + titles.map(_.title.getOrElse("None") + ",children=" + children.map(Reportable.templateNameAndTitle(_)).mkString(","))
  override def toString = "Merged(" + name + ")"
  def templateName = "Merged"
}
case class MergedTitle(title: Option[String], children: List[MergedDescription]) extends ReportableHolder with MergedShortDescription {
  def name = title.getOrElse("None")
}
case class MergedDescription(description: Option[String], children: ReportableList) extends ReportableHolder with MergedShortDescription {
  def name = description.getOrElse("None")
}
