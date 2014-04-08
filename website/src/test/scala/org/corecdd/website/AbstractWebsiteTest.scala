package org.corecdd.website

import java.util.concurrent.locks.ReentrantLock
import scala.xml.NodeSeq.seqToNodeSeq
import org.cddcore.engine._
import org.cddcore.engine.utilities._
import org.cddcore.engine.utilities.Strings
import org.cddcore.engine.UseCasePageChecker
import org.cddcore.engine.WebPageChecker
import org.openqa.selenium.WebDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.BeforeAndAfterAll
import org.scalatest.selenium.WebBrowser
import org.cddcore.engine.Reportable
import org.cddcore.engine.ParamDetails
import org.cddcore.engine.reporting.ReportableToUrl
import org.cddcore.engine.utilities.Strings

object AbstractWebsiteTest {
  val lock = new ReentrantLock();
}
abstract class AbstractWebsiteTest extends AbstractTest with WebBrowser with BeforeAndAfterAll {
  import Reportable._
  val host = "http://localhost:8088"
  def project: Project

  val cddHandler: CddHandler = WebServer.defaultCddHandler(project);
  val server = new WebServer(8088, cddHandler)
  val report = cddHandler.reportCreator.report
  val reportableToUrl = cddHandler.reportCreator.reportableToUrl
  val urlMap = cddHandler.urlMap
  implicit val webDriver: WebDriver = new HtmlUnitDriver

  def projectPageAtIndex = { go to (host); new ProjectPage(List(project, report)) }
  class LivePageChecker(path: ReportableList, val html: String, val reportableToUrl: ReportableToUrl) extends WebPageChecker {
    import ParamDetails._
    val report = path(2).asInstanceOf[Report]
    val engine = PathUtils.findEngine(path)
    new TopLineChecker("Try: " + engine.titleOrDescription(""))
    val engineDivs = divsWith("engine", reportDiv.child)
    assertEquals(1, engineDivs.size)
    def resultTd = only((xml \\ "td").filter(attributeEquals("class", "result")))

    val engineSummaryChecker = new EngineSummaryChecker(path, engineDivs.head, true)
    val hasForm = engine.paramDetails.size == engine.arity
    if (hasForm)
      only((xml \\ "form") filter attributeEquals("class", "paramsForm"))
    else
      assertTextEquals("This engine isn't configured for live operations. Add 'param' details", only((xml \\ "p") filter attributeEquals("class", "notConfigured")))
  }

  class LivePage(val engine: Engine) extends AbstractPage {
    import ParamDetails._
    val path = List(engine, project, report)
    type PageType = LivePage
    val pageChecker = new LivePageChecker(path, pageSource, reportableToUrl)
    def hasForm = pageChecker.hasForm
    def resultTd = pageChecker.resultTd
    def submit = {
      val paramDetails = engine.paramDetails
      val count = paramDetails.foldLeft(0) { (acc, pd) =>
        pd match {
          case ParamDetail(name, _, Some(tv)) =>
            textField(Strings.clean(name)).value = tv
            acc + 1
          case _ => acc
        }
      }
      if (count == paramDetails.size) {
        click on id("submitForm")
        val result = new LivePage(engine)
        val x = result.pageChecker.xml
        Some(result)
      } else None
    }
  }
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

  trait AbstractPage {
    def path: ReportableList
    def clickLogo = { click on id("cddLogo"); new ProjectPage(path.reverse.take(2).reverse) }
    //    println(currentUrl)
  }

  class ProjectPage(val path: ReportableList) extends AbstractPage {
    type PageType = ProjectPage
    val projectPageChecker = new ProjectPageChecker(path, pageSource, reportableToUrl)
    def clickEngine(path: ReportableList) = {
      val urlId = reportableToUrl.urlId(path.head.asInstanceOf[Engine])
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