package org.corecdd.example

import org.cddcore.engine.Project

import org.corecdd.website.AbstractWebsiteTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.corecdd.website.WebServer
import org.cddcore.engine._

@RunWith(classOf[JUnitRunner])
object WebsiteBlankEngineTests extends AbstractWebsiteTest {
  lazy val blankEngine = Engine[String, String]().param((x: String) => x, "Param", "").build
  lazy val project = Project("Example", blankEngine)

  "A blank engine" should "allow live data through it, and report the fact that there was an exception" in {
    projectPageAtIndex
    click on id(urlMap(blankEngine) + "/live")
    val livePage = new LivePage(blankEngine)
    if (livePage.hasForm) {
      livePage.submit match {
        case Some(newLivePage) =>
          import ParamDetails._
          val params = blankEngine.paramDetails.foldLeft(List[Any]())((acc, pd) => pd match { case ParamDetail(_, parser, Some(tv)) => acc :+ parser(tv); case _ => Nil })
          if (params != Nil)
            assertTextEquals(blankEngine.asInstanceOf[EngineWithResult[_]].applyParams(params).toString, newLivePage.resultTd)
        case _ => ;
      }

    }
  }

  def main(args: Array[String]) {
    WebServer(8088, project).launch
  }

}