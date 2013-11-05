package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import scala.xml.NodeSeq
import Reportable._
import org.junit.Assert

@RunWith(classOf[JUnitRunner])
class ReportCreatorTests extends AbstractTest {

  val engine = Engine[Int, String]().title("EngTitle").
    useCase("uc1").
    scenario(10, "ten").expected("10->").
    useCase("uc2").
    scenario(20, "twenty").expected("20->").because((x: Int) => x == 20).
    scenario(21, "twentyone").expected("21->").because((x: Int) => x == 21).
    build
  val pForTitle = Project("ProjName", engine);
  val rc = new ReportCreator(pForTitle, "ReportTitle", new SimpleReportableToUrl)

  "A ReportCreator" should "produce Html for a project " in {
    val html = rc.htmlFor(List(pForTitle)).get
    new ProjectPageChecker(List(pForTitle, rc.report), html, rc.reportableToUrl)
  }

  it should "produce HTML for an engine" in {
    val html = rc.htmlFor(List(engine, pForTitle)).get
    new EnginePageChecker(List(engine, pForTitle, rc.report), html, rc.reportableToUrl)
  }

  it should "produce HTML for usecases" in {
    for (uc <- engine.children) {
      val path = List(uc, engine, pForTitle, rc.report)
      val html = rc.htmlFor(path).get
      new UseCasePageChecker(path, html, rc.reportableToUrl)
    }
  }
  
  it should "produce HTML for scenarios" in {
    for (uc <- engine.children; s <- uc.children) {
      val path = List(s, uc, engine, pForTitle, rc.report)
      val html = rc.htmlFor(path).get
      new ScenarioPageChecker(path, html, rc.reportableToUrl)
    }
  }
}