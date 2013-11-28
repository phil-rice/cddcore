

package org.corecdd.example

import org.cddcore.engine._
import org.cddcore.engine.Test

import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.example.tennisScore.TennisScorer
import org.corecdd.website.AbstractWebsiteTest
import org.corecdd.website.CddHandler
import org.corecdd.website.WebServer
import org.junit.runner.RunWith
import org.openqa.selenium.WebDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.BeforeAndAfterAll
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExampleWebsiteTest extends AbstractWebsiteTest with BeforeAndAfterAll {
  import Reportable._
  val project = Project("Example",
    CategorisePerson.categorise,
    ProcessChequeXml.processCheque,
    TennisScorer.scorer)

  val server = WebServer(project);
  val cddHandler: CddHandler = new CddHandler(project, WebServer.defaultPathHandlers);
  val report = cddHandler.reportCreator.report
  val reportableToUrl = cddHandler.reportCreator.reportableToUrl

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
    clickAllLinks[ProjectPage, EnginePage, EngineFull[_,_]](projectPageAtIndex,
      projectPageToEnginePath,
      click = _.clickEngine(_),
      recover = _.clickLogo)
  }

  it should "allow each UseCase on the project page to be displayed" in {
    clickAllLinks[ProjectPage, UseCasePage, RequirementAndHolder](projectPageAtIndex,
      (pp) => projectPageToEnginePath(pp).flatMap((path) => path.head.asInstanceOf[EngineFull[_,_]].children.map(_ :: path)),
      click = _.clickUseCase(_),
      recover = _.clickLogo)
  }

  it should "allow each scenario on the project page to be display" in {
    clickAllLinks[ProjectPage, ScenarioPage, Test](projectPageAtIndex,
      (pp) => projectPageToEnginePath(pp).flatMap((path) => path.head.asInstanceOf[EngineFull[_,_]].children.flatMap { case uc: RequirementAndHolder => uc.children.map { case s: Test => s :: uc :: path } }),
      click = _.clickScenario(_),
      recover = _.clickLogo)
  }
}