package org.cddcore.website

import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.servlet.ServletHolder
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.server._
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.AbstractHandler
import org.cddcore.engine._
import org.cddcore.utilities.CddDisplayProcessor

object WebServer {
  def defaultPort = {
    val portString = System.getenv("PORT")
    println("PortString[" + portString + "]")
    val port = portString match { case null => 8080; case _ => portString.toInt }
    println("Port[" + port + "]")
    port
  }

  def defaultPathHandlers = List(new FavIconHandler, new RootPathHandler, new PathHandler)

  def apply(engines: List[Engine], title: String = "Engines and Documents", port: Int = defaultPort, handlers: List[CddPathHandler] = defaultPathHandlers)(implicit ldp: CddDisplayProcessor) =
    new WebServer(port, new CddHandler(title, engines, handlers)(ldp))

  def withPreHandlers(port: Int, engines: List[Engine], preHandlers: CddPathHandler*)(implicit ldp: CddDisplayProcessor): WebServer =
    new WebServer(port, new CddHandler("Engines and Documents", engines, preHandlers.toList ::: defaultPathHandlers)(ldp))

  def defaultCddHandler(engines: List[Engine])(implicit ldp: CddDisplayProcessor) = new CddHandler("Engines and Documents", engines, defaultPathHandlers)(ldp)

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


