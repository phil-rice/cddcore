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
  def launch
  def port: Int
}

class CddHandler(p: RequirementAndHolder) extends AbstractHandler {
  val reportCreator = new ReportCreator(p, reportableToUrl = new SimpleReportableToUrl)
  val urlMap = reportCreator.urlMap

  def html(baseRequest: Request, e: Engine, params: List[String], result: String) =
    <div>
      <h2>{ e.titleOrDescription("<Unnamed>") }</h2>
      <form method='post' action={ baseRequest.getUri().getPath() }>
        {
          for (i <- 0 to (e.arity - 1)) yield {
            val name = "param" + i; <label id={ name }>{ name } </label>
                                    <input name={ name } id={ name } type='text' value={ params(i) }/><br/>
          }
        }
        <input type='submit'/>
      </form>
      Result:{ result }
    </div>.toString

  def getForm(baseRequest: Request, request: HttpServletRequest, e: Engine) =
    html(baseRequest, e, List("").padTo(e.arity, ""), "")

  def postForm(baseRequest: Request, request: HttpServletRequest, e: Engine) = {
    try {
      val paramStrings =
        for (i <- 0 to e.arity - 1)
          yield request.getParameterValues(s"param$i")(0)

      val params = paramStrings.zip(e.parsers).map { case (param, parser) => parser(param) }.toList
      val conclusion = e.findConclusionFor(params)
      val result = e.evaluateConclusion(params, conclusion)
      val aTest = conclusion.scenarios.head
      val engineForm = html(baseRequest, e, paramStrings.toList, "Result: " + params + "\n" + result )
      HtmlRenderer.liveEngineHtml(reportCreator.rootUrl, Some(aTest), Set(), engineForm).render(reportCreator.reportableToUrl, urlMap, Report("Try: " + e.titleOrDescription(""), e))
    } catch { case t: Throwable => t.printStackTrace(); html(baseRequest, e, List("").padTo(e.arity, ""), e.getClass + ": " + t.getMessage()) }
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
  def launch {
    val server = new Server(port);
    server.setHandler(handler);
    server.start();
    server.join();
  }
}

class WebServerFromPackage(val port: Int, packages: List[String]) extends WebServer {

  def launch() {
    val server = new Server(port)
    val connector = new SelectChannelConnector()
    server.addConnector(connector)
    val holder: ServletHolder = new ServletHolder(classOf[ServletContainer])
    holder.setInitParameter("com.sun.jersey.config.property.resourceConfigClass", "com.sun.jersey.api.core.PackagesResourceConfig")
    holder.setInitParameter("com.sun.jersey.config.property.packages", packages.mkString(";"))
    val context = new ServletContextHandler(server, "/", ServletContextHandler.SESSIONS)
    context.addServlet(holder, "/*")
    server.start
    server.join
  }

}
