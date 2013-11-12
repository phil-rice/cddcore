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

  def defaultPathHandlers = List(new FavIconHandler, new RootPathHandler, new LivePathHandler, new PathHandler)
  
  def apply(port: Int, p: RequirementAndHolder, handlers: List[CddPathHandler] = defaultPathHandlers): WebServer = new WebServer(port, new CddHandler(p, handlers))
  def withPreHandlers(port: Int, p: RequirementAndHolder, handlers: CddPathHandler*): WebServer = new WebServer(port, new CddHandler(p, handlers.toList ::: defaultPathHandlers))
  def apply( p: RequirementAndHolder): WebServer = new WebServer(defaultPort, new CddHandler(p, defaultPathHandlers))

}

class WebServer(val port: Int, val handler: Handler) {

  val server = new Server(port);
  server.setHandler(handler)
  def launch {
    start; server.join
  }
  def start = server.start
  def stop = server.stop
}


