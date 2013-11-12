package org.corecdd.website

import org.cddcore.engine._

import org.cddcore.engine.RequirementAndHolder
import org.eclipse.jetty.server._
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.servlet._
import com.sun.jersey.spi.container.servlet.ServletContainer
import javax.servlet.http._
import scala.xml.Elem

class HandlerContext(val root: RequirementAndHolder, val reportCreator: ReportCreator[SimpleReportableToUrl], val method: String, val fullUri: String, val uriPath: String) {
  import PathUtils._
  val reportableToUrl = reportCreator.reportableToUrl
  val urlMap = reportCreator.urlMap
  lazy val path: List[Reportable] = urlMap(uriPath)
  lazy val engine = findEngine(path)
  def toHtmlString(o: Any) = o match { case h: HtmlDisplay => h.htmlDisplay; case _ => o.toString }
}

trait CddPathHandler {
  def willHandle(uri: String): Boolean
  def findUriPath(uri: String): String = uri
  def paramsINeed(context: HandlerContext): List[String] = List()
  def html(context: HandlerContext, params: List[(String, String)]): String
  def getParam(params: List[(String, String)], name: String) = params.find(_._1 == name).getOrElse(throw new IllegalArgumentException(name))._2
}

case class Param(name: String, valueAsString: String, value: Any)

class CddHandler(p: RequirementAndHolder, pathHandlers: List[CddPathHandler]) extends AbstractHandler {
  val reportCreator = new ReportCreator(p, title = null, live = true, reportableToUrl = new SimpleReportableToUrl)
  val urlMap = reportCreator.urlMap

  def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
    val uri = baseRequest.getUri();
    val path = uri.getPath
    pathHandlers.find(_.willHandle(path)) match {
      case Some(ph) =>
        try {
          baseRequest.setHandled(true);
          response.setContentType("text/html;charset=utf-8");
          response.setStatus(HttpServletResponse.SC_OK);
          val context = new HandlerContext(p, reportCreator, baseRequest.getMethod(), path, ph.findUriPath(path))
          val paramsINeed = ph.paramsINeed(context)
          val paramsNameAndValue = paramsINeed.map((name) => (name, baseRequest.getParameter(name)))
          val html = ph.html(context, paramsNameAndValue)
          response.getWriter().println(html)
        } catch { case e: Throwable => println(ph); e.printStackTrace(); throw e }
      case _ => ;
    }
  }
}

class RootPathHandler extends CddPathHandler {
  def willHandle(uri: String): Boolean = uri == "/"
  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    reportCreator.htmlFor(List(root, reportCreator.report)).get
  }
}
class FavIconHandler extends CddPathHandler {
  def willHandle(uri: String): Boolean = uri == "/favicon.ico"
  def html(context: HandlerContext, params: List[(String, String)]): String = ""
}

class PathHandler extends CddPathHandler {
  def willHandle(uri: String): Boolean = true
  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    val x = urlMap.get(uriPath)
    x.flatMap(reportCreator.htmlFor(_)).get
  }
}

class LivePathHandler extends CddPathHandler {
  def willHandle(uri: String) = uri.endsWith("/live")
  override def findUriPath(uri: String): String = uri.substring(0, uri.length() - 5)
  override def paramsINeed(context: HandlerContext) = {
    import context._
    engine.paramDetails.map((x) => Some(x.displayName)).padTo(engine.arity, None).zipWithIndex.map { case (None, i) => "param" + i; case (Some(x), _) => x }
  }

  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    val (engineForm, paramNameAndValues, conclusion) = try {
      val paramNameAndValues = params.zip(engine.paramDetails).map { case ((paramName, paramString), details) => Param(paramName, paramString, details.parser(paramString)) }.toList
      val engineParams = paramNameAndValues.map(_.value)
      val conclusion = engine.findConclusionFor(engineParams)
      val result = engine.evaluateConclusion(engineParams, conclusion)
      (formHtml(context, paramNameAndValues, result.toString), Some(engineParams), Some(conclusion))
    } catch { case t: Throwable => t.printStackTrace(); (formHtml(context, params.map((n) => Param(n._1, n._2, "")), t.getClass + ": " + t.getMessage()), None, None) }
    HtmlRenderer(true).liveEngineHtml(reportCreator.rootUrl, paramNameAndValues, conclusion, Set(), engineForm).render(reportCreator.reportableToUrl, urlMap, Report("Try: " + engine.titleOrDescription(""), engine))
  }

  def formHtml(context: HandlerContext, paramNameAndValues: List[Param], result: String) = {
    import context._
    val form =
      <form method='post' action={ fullUri }>
        {
          for (i <- 0 to (engine.arity - 1)) yield {
            val name: String = paramNameAndValues(i).name;
            <label id={ name }>{ name } </label>
            <input name={ name } id={ name } type='text' value={ paramNameAndValues(i).valueAsString }/><br/>
          }
        }
        <input type='submit'/>
      </form>
    val center = if (engine.arity == engine.paramDetails.size) form else <p>This engine isn't configured for live operations. Add 'param' details</p>;
    <div>
      { center }
      <br/>
      <table>
        <tr><td>Params:</td><td>{ paramNameAndValues.map((p) => toHtmlString(p.value)).mkString(", ") }</td></tr>
        <tr><td>Result:</td><td>{ result }</td></tr>
      </table>
    </div>.toString
  }
}
