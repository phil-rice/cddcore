package org.cddcore.website

import java.util.concurrent.locks.ReentrantLock

import scala.xml.NodeSeq.seqToNodeSeq
import org.cddcore.engine._
import org.cddcore.utilities._
import org.openqa.selenium.WebDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.BeforeAndAfterAll
import org.scalatest.selenium.WebBrowser
import org.cddcore.engine.Reportable
import org.cddcore.engine._
import org.cddcore.htmlRendering._
import java.util.Date
import ReportableHelper._

object AbstractWebsiteTest {
  val lock = new ReentrantLock();
}
abstract class AbstractWebsiteTest extends AbstractTest with WebBrowser with BeforeAndAfterAll {
  import Reportable._
  def engines: List[Engine]

  val host = "http://localhost:8088"

  val cddHandler: CddHandler = WebServer.defaultCddHandler(engines);
  val server = new WebServer(8088, cddHandler)
  val urlMap = cddHandler.urlMap
  implicit val webDriver: WebDriver = new HtmlUnitDriver

  def projectPageAtIndex = { go to (host); new IndexPage }

  override def beforeAll {
    super.beforeAll
    AbstractWebsiteTest.lock.lock()
    server.start
  }

  override def afterAll {
    super.afterAll
    server.stop
    AbstractWebsiteTest.lock.unlock()
  }

  trait AbstractPage extends HtmlRenderedChecker {
    def html = pageSource
    val renderContext = RenderContext(AbstractWebsiteTest.this.urlMap, new Date(), "")
    val expectedTitle = "Project: "
    def clickLogo = { click on id("cddLogo"); new IndexPage() }
    checkTopLine(expectedTitle)

    def checkPath(path: List[Reportable]) =
      for (subPath <- Lists.decreasingList(path)) subPath.head match {
        case e: EngineDescription[_, _] =>
          val engineWithTestsDiv = onlyDivWith("engineWithTests")
          checkEngineSummary(subPath, engineWithTestsDiv)
        case u: UseCase[_, _] =>
          val usecaseDiv = onlyDivWith("usecaseSummary")
          checkUsecase(u, usecaseDiv)
        case s: AnyScenario =>
          val scenarioDiv = onlyDivWith("scenario")
          checkScenarioDetails(subPath, scenarioDiv)
      }

  }

  class IndexPage extends AbstractPage {
    type PageType = IndexPage

    def clickEngineFromTests(path: List[Reportable]) = {
      val urlId = UrlMap.urlId(path.head.asInstanceOf[EngineRequirement[_, _]])
      val h = html
      click on id(urlId)
      new EngineFromTestsPage(path)
    }

    def clickUseCase(path: List[Reportable]) = {
      val urlId = UrlMap.urlId(path.head.asInstanceOf[UseCase[_, _]])
      click on id(urlId)
      new UseCasePage(path)
    }

    def clickScenario(path: List[Reportable]) = {
      val urlId = UrlMap.urlId(path.head.asInstanceOf[AnyScenario])
      click on id(urlId)
      new ScenarioPage(path)
    }
  }

  class EngineFromTestsPage(val path: List[Reportable]) extends AbstractPage {
    type PageType = EngineFromTestsPage
    checkPath(path)
    val ed = path.head.asInstanceOf[EngineRequirement[_, _]]
    val useCases = ed.useCases
    val useCaseDivs = divsWith("usecaseSummary")
    for ((uc, div) <- useCases.zipAll(useCaseDivs, null, null))
      checkUsecaseWithScenariosSummarized(uc :: path, div)
  }

  class UseCasePage(val path: List[Reportable]) extends AbstractPage {
    type PageType = UseCasePage
    checkPath(path)
    val usecaseDiv = onlyDivWith("usecaseSummary")
    checkUsecaseWithScenariosDetails(path, usecaseDiv)
  }

  class ScenarioPage(val path: List[Reportable]) extends AbstractPage {
    type PageType = ScenarioPage
    checkPath(path)
    val scenarioDiv = onlyDivWith("scenario")
    checkScenarioDetails(path, scenarioDiv)

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

