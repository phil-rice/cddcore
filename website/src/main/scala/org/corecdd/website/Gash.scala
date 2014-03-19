package org.corecdd.website

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.Handler
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpServletRequest
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler

object Gash {
  val handler = new AbstractHandler {
    def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
      baseRequest.setHandled(true);
      response.setContentType("text/html;charset=utf-8");
      response.setStatus(HttpServletResponse.SC_OK);
      response.getWriter().println("Hello world")
    }
  }

  def main(args: Array[String]) {
    val s = new Server(8080)
    s.setHandler(handler)
    s.start
    s.join
  }
}