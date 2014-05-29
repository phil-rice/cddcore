package org.cddcore.website

import org.cddcore.engine._
import scala.language.implicitConversions
import org.eclipse.jetty.server._
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.servlet._
import scala.xml.Elem
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpServletRequest
import org.cddcore.utilities._
import org.cddcore.htmlRendering._
import java.util.Date
import org.cddcore.htmlRendering.UrlMap
import org.cddcore.engine.PathUtils
import org.cddcore.htmlRendering.Report

class HandlerContext(val renderContext: RenderContext, val engines: List[Engine], val title: String, val method: String, val fullUri: String, val uriPath: String) {
  import PathUtils._
  val urlMap = renderContext.urlMap
  lazy val path: List[Reportable] = urlMap(uriPath)
  lazy val engine = findEngine(path)
}

trait CddPathHandler {
  def willHandle(uri: String): Boolean
  def findUriPath(uri: String): String = uri
  def paramsINeed(context: HandlerContext): List[String] = List()
  def html(context: HandlerContext, params: List[(String, String)]): String
  def getParam(params: List[(String, String)], name: String) = params.find(_._1 == name).getOrElse(throw new IllegalArgumentException(name))._2
}

case class Param(name: String, valueAsString: String, value: Any)

class CddHandler(title: String, engines: List[Engine], pathHandlers: List[CddPathHandler], val prefix: String = "")(implicit ldp: CddDisplayProcessor) extends AbstractHandler {
  import EngineTools._
  val urlMap = UrlMap() ++ engines
  if (Engine.logging)
    for (e <- engines)
      println(s"CDD Hander. Engine: ${e.textOrder}. Requirements ${e.asRequirement.textOrder}")

  def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
    val renderContext = RenderContext(urlMap, new Date(), prefix + "/")
    val uri = baseRequest.getUri();
    val fullPath = uri.getPath
    if (fullPath.startsWith(prefix)) {
      val path = fullPath.substring(prefix.length())
      pathHandlers.find(_.willHandle(path)) match {
        case Some(ph) =>
          try {
            baseRequest.setHandled(true);
            response.setContentType("text/html;charset=utf-8");
            response.setStatus(HttpServletResponse.SC_OK);
            val context = new HandlerContext(renderContext, engines, title, baseRequest.getMethod(), path, ph.findUriPath(path))
            val paramsINeed = ph.paramsINeed(context)
            val paramsNameAndValue = paramsINeed.map((name) => (name, baseRequest.getParameter(name)))
            val html = ph.html(context, paramsNameAndValue)
            response.getWriter().println(html)
          } catch {
            case e: Throwable =>
//              println(ph);
//              e.printStackTrace();
              throw e
          }
        case _ => ;
      }
    }
  }
}

class RootPathHandler extends CddPathHandler {
  def willHandle(uri: String): Boolean = uri == "/"
  def html(context: HandlerContext, params: List[(String, String)]): String = {
    import context._
    import renderContext._
    val report = Report.documentAndEngineReport(None, reportDate, engines)
    val html = Report.html(report, HtmlRenderer.engineAndDocumentsSingleItemRenderer, renderContext)
    html
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
    val report = Report.focusedReport(None, path)
    val renderer = Report.rendererFor(report)
    val ed = PathUtils.findEngine(path)
    val rc = (path.head, ed) match {
      case (s: Scenario[_, _, _, _], ed: EngineDescription[_,_,_,_]) => {
        val engine = ""
        renderContext
      }
      case _ => renderContext
    }
    Report.html(report, renderer, rc)
  }
}
//      case _ => List()
//    }
//  }
//
//  def html(context: HandlerContext, params: List[(String, String)]): String = {
//    import context._
//    val (engineForm, paramNameAndValues, conclusion) = try {
//      val paramNameAndValues = engine match {
//        case pd: ParamDetails =>
//          params.zip(engine.paramDetails).map {
//            case ((paramName, paramString), details) => Param(paramName, paramString, details.parser(paramString))
//          }.toList
//        case _ => List()
//      }
//      val engineParams = paramNameAndValues.map(_.value)
//      engine match {
//        case e: EngineBuiltFromTests[_] if paramNameAndValues.size == e.arity => {
//          val conclusion = e.findConclusionFor(engineParams)
//          val result = e.evaluateConclusionNoException(engineParams, conclusion)
//          (formHtml(context, paramNameAndValues, result.toString), Some(engineParams), Some(conclusion))
//        }
//        case _ => {
//          val result = "Live execution on engine with children not yet supported"
//          (formHtml(context, paramNameAndValues, result.toString), Some(engineParams), None)
//        }
//      }
//    } catch { case t: Throwable => t.printStackTrace(); (formHtml(context, params.map((n) => Param(n._1, n._2, "")), t.getClass + ": " + t.getMessage()), None, None) }
//    HtmlRenderer(CddDisplayProcessor, true).liveEngineHtml(reportCreator.rootUrl, paramNameAndValues, conclusion, Set(), engineForm).render(reportCreator.reportableToUrl, urlMap,
//      Report("Try: " + engine.titleOrDescription(""),
//        engine))
//  }
//
//  def formHtml(context: HandlerContext, paramNameAndValues: List[Param], result: String) = {
//    import context._
//    val center = if (engine.arity == paramNameAndValues.size)
//      <form class='paramsForm' method='post' action={ fullUri }>
//        {
//          for (i <- 0 to (engine.arity - 1)) yield {
//            val name: String = paramNameAndValues(i).name;
//            val cleanedName: String = Strings.clean(name)
//            <label id={ cleanedName }>{ name } </label>
//            <input name={ cleanedName } id={ cleanedName } type='text' value={ paramNameAndValues(i).valueAsString }/><br/>
//          }
//        }
//        <input id='submitForm' type='submit'/>
//      </form>
//    else
//      <p class='notConfigured'>This engine isn't configured for live operations. Add 'param' details</p>;
//
//    <div>
//      { center }
//      <br/>
//      <table>
//        <tr><td>Params:</td><td>{ paramNameAndValues.map((p) => toHtmlString(p.value)).mkString(", ") }</td></tr>
//        <tr><td>Result:</td><td class='result'>{ result }</td></tr>
//      </table>
//    </div>.toString
//  }
//}
