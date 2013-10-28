package org.corecdd.website

import org.eclipse.jetty.servlet._
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server._
import javax.ws.rs._
import com.sun.jersey.spi.container.servlet.ServletContainer
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.AbstractHandler
import javax.servlet.http._
import org.cddcore.engine._
import org.cddcore.engine.RequirementAndHolder

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

  def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) = {
    response.setContentType("text/html;charset=utf-8");
    response.setStatus(HttpServletResponse.SC_OK);
    baseRequest.setHandled(true);
    val uri = baseRequest.getUri();
    val path = uri.getPath
    val html = path match {
      case "/" => reportCreator.htmlFor(List(p, reportCreator.report))
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
