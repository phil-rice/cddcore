package org.cddcore.engine

import org.antlr.stringtemplate.AttributeRenderer
import org.joda.time.format.DateTimeFormat
import org.antlr.stringtemplate.StringTemplate
import org.cddcore.engine.tests.CddRunner
import java.io.File

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

  protected def render(reportableToUrl: ReportableToUrl, urlMap: Map[Reportable, String], path: List[Reportable], templateName: String): String = {
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
    }
  })
  protected def engineConfig = RenderAttributeConfigurer[Engine]("Engine", (_, _, path, e, stringTemplate) => stringTemplate.setAttribute("decisionTreeNodes", e.decisionTreeNodes))
  def decisionTreeConfig(test: Option[Test]) =
    RenderAttributeConfigurer[Engine]("Engine", (reportableToUrl, urlMap, path, e, stringTemplate) =>
      stringTemplate.setAttribute("decisionTree", e.toStringWith(test match {
        case Some(t) => new HtmlWithTestIfThenPrinter(t, reportableToUrl, urlMap)
        case _ => new HtmlIfThenPrinter(reportableToUrl, urlMap)
      })))

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
      stringTemplate.setAttribute("params", ValueForRender(t.paramPrinter(p)))
  })

}

trait Renderer {
  def clear;

  def render(reportableToUrl: ReportableToUrl, urlMap: Map[Reportable, String], path: List[Reportable]): String
}

object RenderAttributeConfigurer {
  import Reportable._
  def apply(fn: (ReportableToUrl, UrlMap, ReportableList, StringTemplate) => Unit) = BaseRenderAttributeConfigurer(fn)
  def apply[R <: Reportable](templateName: String, fn: (ReportableToUrl, UrlMap, ReportableList, R, StringTemplate) => Unit) = TypedRenderAttributeConfigurer[R](templateName, fn)

  case class BaseRenderAttributeConfigurer(val fn: (ReportableToUrl, Map[Reportable, String], List[Reportable], StringTemplate) => Unit) extends RenderAttributeConfigurer {
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

  def render(reportableToUrl: ReportableToUrl, urlMap: Map[Reportable, String], path: ReportableList): String = {
    val result = stringTemplate.toString
    result
  }
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
