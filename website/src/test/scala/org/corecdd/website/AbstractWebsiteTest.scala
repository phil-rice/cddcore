package org.corecdd.website

import org.cddcore.engine.AbstractTest

import org.cddcore.engine._
import org.openqa.selenium.WebDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.BeforeAndAfterAll
import org.scalatest.selenium.WebBrowser

abstract class AbstractWebsiteTest extends AbstractTest with WebBrowser with BeforeAndAfterAll {
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

  trait AbstractPage {
    def path: ReportableList
    def clickLogo = { click on id("cddLogo"); new ProjectPage(path.reverse.take(2).reverse) }
//    println(currentUrl)
  }

  class ProjectPage(val path: ReportableList) extends AbstractPage {
    type PageType = ProjectPage
    val projectPageChecker = new ProjectPageChecker(path, pageSource, reportableToUrl)
    def clickEngine(path: ReportableList) = {
      val urlId = reportableToUrl.urlId(path.head.asInstanceOf[EngineFull[_,_]])
      click on id(urlId)
      new EnginePage(path)
    }
    
    def clickUseCase(path: ReportableList) = {
      val urlId = reportableToUrl.urlId(path.head)
      click on id(urlId)
      new UseCasePage(path)
    }
    
    def clickScenario(path: ReportableList) = {
      val urlId = reportableToUrl.urlId(path.head)
      click on id(urlId)
      new ScenarioPage(path)
    }
  }

  class EnginePage(val path: ReportableList) extends AbstractPage {
    type PageType = EnginePage
    val enginePageChecker = new EnginePageChecker(path, pageSource, reportableToUrl)

  }

  class UseCasePage(val path: ReportableList) extends AbstractPage {
    type PageType = UseCasePage
    val usePageChecker = new UseCasePageChecker(path, pageSource, reportableToUrl)
  }

  class ScenarioPage(val path: ReportableList) extends AbstractPage {
    type PageType = ScenarioPage
    new ScenarioPageChecker(path, pageSource, reportableToUrl)

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