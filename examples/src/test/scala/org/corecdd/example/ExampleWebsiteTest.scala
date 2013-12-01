

package org.corecdd.example

import org.cddcore.engine._

import org.cddcore.engine.Test
import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.example.tennisScore.TennisScorer
import org.corecdd.website.AbstractWebsiteTest
import org.corecdd.website.CddHandler
import org.junit.runner.RunWith
import org.openqa.selenium.WebDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.BeforeAndAfterAll
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.EngineWithResult
import org.corecdd.website.WebServer

@RunWith(classOf[JUnitRunner])
class ExampleWebsiteTest extends AbstractWebsiteTest with BeforeAndAfterAll {
  import Reportable._
  val project = Project("Example",
    CategorisePerson.categorise,
    ProcessChequeXml.processCheque,
    TennisScorer.scorer)

  val server = WebServer(project);
  val cddHandler: CddHandler = WebServer.defaultCddHandler(project);
  val report = cddHandler.reportCreator.report
  val reportableToUrl = cddHandler.reportCreator.reportableToUrl
  val urlMap = cddHandler.urlMap

  def projectPageAtIndex = { go to (host); new ProjectPage(List(project, report)) }

  "The example website" should "display the project page if no page specified" in {
    projectPageAtIndex
  }

  it should "go to the project page if cdd logo clicked" in {
    projectPageAtIndex.clickLogo
  }

  def clickAllLinks[Main <: AbstractPage, Checked <: AbstractPage, R <: Reportable](
    startPage: Main,
    reportableListGetter: (Main) => List[ReportableList],
    click: (Main, ReportableList) => Checked,
    recover: (Checked) => Main) = {

    var here = startPage
    for (r <- reportableListGetter(startPage)) {
      val newPage = click(here, r)
      here = recover(newPage)
    }
  }

  def projectPageToEnginePath: (ProjectPage) => List[ReportableList] = (pp) => pp.projectPageChecker.engineSectionCheckers.map((esc) => List[Reportable](esc.engine, project, report))

  it should "allow each engine to be display" in {
    clickAllLinks[ProjectPage, EnginePage, EngineFull[_, _]](projectPageAtIndex,
      projectPageToEnginePath,
      click = _.clickEngine(_),
      recover = _.clickLogo)
  }

  it should "allow each UseCase on the project page to be displayed" in {
    clickAllLinks[ProjectPage, UseCasePage, RequirementAndHolder](projectPageAtIndex,
      (pp) => projectPageToEnginePath(pp).flatMap((path) => path.head.asInstanceOf[Engine].children.map(_ :: path)),
      click = _.clickUseCase(_),
      recover = _.clickLogo)
  }

  it should "allow each scenario on the project page to be displayed" in {
    clickAllLinks[ProjectPage, ScenarioPage, Test](projectPageAtIndex,
      (pp) => projectPageToEnginePath(pp).flatMap((path) => path.head.asInstanceOf[Engine].children.flatMap { case uc: RequirementAndHolder => uc.children.map { case s: Test => s :: uc :: path } }),
      click = _.clickScenario(_),
      recover = _.clickLogo)
  }

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

  it should "allow the live button to be displayed" in {
    for (e: Engine <- project.collect { case e: Engine => e }) {
      projectPageAtIndex
      click on id(urlMap(e) + "/live")
      val livePage = new LivePage(e)
      if (livePage.hasForm) {
        livePage.submit match {
          case Some(newLivePage) =>
            import ParamDetails._
            val params = e.paramDetails.foldLeft(List[Any]())((acc, pd) => pd match { case ParamDetail(_, parser, Some(tv)) => acc :+ parser(tv); case _ => Nil })
            if (params != Nil)
              assertTextEquals( e.asInstanceOf[EngineWithResult[_]].applyParams(params).toString, newLivePage.resultTd)
          case _ => ;
        }

      }
    }
  }

}