package org.corecdd.website

import org.cddcore.engine.AbstractTest
import org.junit.runner.RunWith
import org.scalatest.selenium.HtmlUnit
import org.scalatest.selenium.WebBrowser
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.openqa.selenium.WebDriver
import org.scalatest.BeforeAndAfterAll
import scala.xml.Node
import org.cddcore.engine.Reportable
import org.cddcore.engine.Project
import org.cddcore.engine.ReportableToUrl
import org.cddcore.engine.Strings
import scala.xml.XML
import scala.xml.NodeSeq
import org.cddcore.engine.Report
import org.cddcore.engine.Engine
import scala.xml.Elem
import org.cddcore.engine.RequirementAndHolder
import org.cddcore.engine.Test
import org.cddcore.engine.ReportPages

abstract class AbstractWebsiteTest extends AbstractTest with WebBrowser with BeforeAndAfterAll with ReportPages {
  import Reportable._
  val host = "http://localhost:8080"
  def server: WebServer
  def reportableToUrl: ReportableToUrl

  implicit val webDriver: WebDriver = new HtmlUnitDriver

  override def beforeAll {
    super.beforeAll
    server.start
  }

  override def afterAll {
    super.afterAll
    server.stop
  }

  class ProjectPage(path: ReportableList) {
    type PageType = ProjectPage
    val project = findProject(path)
    val reportSection = new ReportSection(path, XML.loadString(pageSource), reportableToUrl)
    def clickLogo = { click on id("cddLogo"); new ProjectPage(path) }
    def clickUsecase(useCase: RequirementAndHolder) = {
    }
  }

  def findUseCase(path: ReportableList) = usecasePath(path).head.asInstanceOf[RequirementAndHolder]
  def usecasePath(path: ReportableList): ReportableList = path match {
    case (usecase: RequirementAndHolder) :: tail => path
    case h :: tail => usecasePath(tail)
    case _ => throw new IllegalArgumentException
  }

  def findEngine(path: ReportableList) = enginePath(path).head.asInstanceOf[Engine]
  def enginePath(path: ReportableList): ReportableList = path match {
    case (engine: Engine) :: tail => path
    case h :: tail => enginePath(tail)
    case _ => throw new IllegalArgumentException
  }
  def findProject(path: ReportableList) = projectPath(path).head.asInstanceOf[Project]
  def projectPath(path: ReportableList): ReportableList = path match {
    case (project: Project) :: tail => path
    case h :: tail => projectPath(tail)
    case _ => throw new IllegalArgumentException
  }
  def findReport(path: ReportableList) = reportPath(path).head.asInstanceOf[Report]
  def reportPath(path: ReportableList): ReportableList = path match {
    case (project: Report) :: tail => path
    case h :: tail => reportPath(tail)
    case _ => throw new IllegalArgumentException
  }

  def findWithClass(tag: String, clazzName: String) = findAll(tagName(tag)).filter(attributeEquals("class", clazzName))

  def attributeEquals(name: String, value: String)(element: Element) = {
    val n = element;
    val attribute = element.attribute(name)
    val filtered = attribute.filter(
      (v) =>
        v.toString == value)
    filtered.isDefined
  }

  def only[T](iterator: Iterator[T]) = { val result = iterator.next; assert(!iterator.hasNext); result }
  def onlyDiv(className: String) = only[Element](findAll(tagName("div")).filter(attributeEquals("class", className)))
  /** The implementation of this is cheesy. It just checks that <div class='xxx' is present inside the outer elements text */
}