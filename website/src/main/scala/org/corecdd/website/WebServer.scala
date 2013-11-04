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

object WebServer {
  def defaultPort = {
    val portString = System.getenv("PORT")
    println("PortString[" + portString + "]")
    val port = portString match { case null => 8080; case _ => portString.toInt }
    println("Port[" + port + "]")
    port
  }
  def apply(port: Int, packages: String*): WebServer = new WebServerFromPackage(port, packages.toList)

  def apply(handler: Handler, port: Int = defaultPort): WebServer = new WebServerWithHandler(port, handler)

  def apply(packages: String*): WebServer = {
    apply(defaultPort, packages: _*)
  }
}

abstract class WebServer {
  val server = new Server(port);
  def launch {
    start; server.join
  }
  def start
  def stop = server.stop
  def port: Int
}

class CddHandler(p: RequirementAndHolder) extends AbstractHandler {
  val reportCreator = new ReportCreator(p, reportableToUrl = new SimpleReportableToUrl)
  val urlMap = reportCreator.urlMap

  def findParamNames(e: Engine) = e.paramDetails.map((x) => Some(x.displayName)).padTo(e.arity, None).zipWithIndex.map { case (None, i) => "param" + i; case (Some(x), _) => x }
  def formHtml(baseRequest: Request, e: Engine, params: List[String], result: String) = {
    val paramNames = findParamNames(e)
    <div>
      <h2>{ e.titleOrDescription("<Unnamed>") }</h2>
      <form method='post' action={ baseRequest.getUri().getPath() }>
        {
          for (i <- 0 to (e.arity - 1)) yield {
            val name: String = paramNames(i);
            <label id={ name }>{ name } </label>
            <input name={ name } id={ name } type='text' value={ params(i) }/><br/>
          }
        }
        <input type='submit'/>
      </form>
      { result }
    </div>.toString
  }

  def getForm(baseRequest: Request, request: HttpServletRequest, e: Engine) =
    html(baseRequest, request, e, List().padTo(e.arity, ""))

  def toString(o: Any) = o match {
    case h: HtmlDisplay => h.htmlDisplay
    case _ => "" + o
  }
  def postForm(baseRequest: Request, request: HttpServletRequest, e: Engine) = {
    val paramNames = findParamNames(e)
    val paramStrings =
      for (i <- 0 to e.arity - 1)
        yield request.getParameterValues(paramNames(i))(0)
    html(baseRequest, request, e, paramStrings)

  }

  def html(baseRequest: Request, request: HttpServletRequest, e: Engine, paramStrings: Iterable[String]) = {
    val (engineForm, test) = try {
      val params = paramStrings.zip(e.paramDetails).map { case (param, details) => details.parser(param) }.toList
      val conclusion = e.findConclusionFor(params)
      val result = e.evaluateConclusion(params, conclusion)
      val aTest = conclusion.scenarios.head
      (formHtml(baseRequest, e, paramStrings.toList, "Params: " + params.map(toString(_)) + "\n" + result), Some(aTest))
    } catch { case t: Throwable => t.printStackTrace(); (formHtml(baseRequest, e, List("").padTo(e.arity, ""), t.getClass + ": " + t.getMessage()), None) }
    HtmlRenderer.liveEngineHtml(reportCreator.rootUrl, test, Set(), engineForm).render(reportCreator.reportableToUrl, urlMap, Report("Try: " + e.titleOrDescription(""), e))
  }

  def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) = {
    response.setContentType("text/html;charset=utf-8");
    response.setStatus(HttpServletResponse.SC_OK);
    baseRequest.setHandled(true);
    val uri = baseRequest.getUri();
    val path = uri.getPath
    val html = path match {
      case s: String if s.endsWith("/try") =>
        val withoutTry = s.substring(0, s.length - 4);
        val path: List[Reportable] = urlMap(withoutTry)
        path.head match {
          case e: Engine =>
            if (request.getMethod().equalsIgnoreCase("GET"))
              Some(getForm(baseRequest, request, e));
            else if (request.getMethod().equalsIgnoreCase("POST"))
              Some(postForm(baseRequest, request, e));
        }
      case "/" =>
        reportCreator.htmlFor(List(p, reportCreator.report))
      case p =>
        urlMap.get(p).flatMap(reportCreator.htmlFor(_))
    }
    html match {
      case Some(h) => response.getWriter().println(h);
      case _ => ;
    }
  }
}

class WebServerWithHandler(val port: Int, handler: Handler) extends WebServer {
  def start {
    server.setHandler(handler);
    server.start();
  }
}

class WebServerFromPackage(val port: Int, packages: List[String]) extends WebServer {

  def start() {
    val connector = new SelectChannelConnector()
    server.addConnector(connector)
    val holder: ServletHolder = new ServletHolder(classOf[ServletContainer])
    holder.setInitParameter("com.sun.jersey.config.property.resourceConfigClass", "com.sun.jersey.api.core.PackagesResourceConfig")
    holder.setInitParameter("com.sun.jersey.config.property.packages", packages.mkString(";"))
    val context = new ServletContextHandler(server, "/", ServletContextHandler.SESSIONS)
    context.addServlet(holder, "/*")
    server.start
  }

}
