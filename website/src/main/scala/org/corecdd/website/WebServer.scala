package org.corecdd.website

import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.servlet.ServletHolder
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.server._
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.AbstractHandler
import com.sun.jersey.spi.container.servlet.ServletContainer
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
  def apply(port: Int, p: RequirementAndHolder, handlers: CddPathHandler*): WebServer = new WebServerWithHandler(port, new CddHandler(p, handlers.toList))
  def apply(p: RequirementAndHolder): WebServer = apply(defaultPort, p, new FavIconHandler, new RootPathHandler, new LivePathHandler, new PathHandler)

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

class WebServerWithHandler(val port: Int, val handler: Handler) extends WebServer {
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
