package org.corecdd.website

import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.servlet.ServletHolder
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.server._
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.AbstractHandler
import org.cddcore.engine.RequirementAndHolder
import org.cddcore.engine.LoggerDisplayProcessor

object WebServer {
  def defaultPort = {
    val portString = System.getenv("PORT")
    println("PortString[" + portString + "]")
    val port = portString match { case null => 8080; case _ => portString.toInt }
    println("Port[" + port + "]")
    port
  }

  def defaultPathHandlers = List(new FavIconHandler, new RootPathHandler, new LivePathHandler, new PathHandler)
  def defaultLoggerDisplayProcessor = LoggerDisplayProcessor()
  def apply(port: Int, p: RequirementAndHolder, handlers: List[CddPathHandler] = defaultPathHandlers): WebServer = new WebServer(port, new CddHandler(defaultLoggerDisplayProcessor, p, handlers))
  def withPreHandlers(port: Int, p: RequirementAndHolder, handlers: CddPathHandler*): WebServer = new WebServer(port, new CddHandler(defaultLoggerDisplayProcessor, p, handlers.toList ::: defaultPathHandlers))
  def defaultCddHandler(p: RequirementAndHolder) =  new CddHandler(defaultLoggerDisplayProcessor, p, defaultPathHandlers)
  def apply( p: RequirementAndHolder): WebServer = new WebServer(defaultPort, defaultCddHandler(p))
  def apply( handler: CddHandler): WebServer = new WebServer(defaultPort, handler)

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


