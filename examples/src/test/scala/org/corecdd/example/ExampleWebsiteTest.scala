

package org.corecdd.example

import org.cddcore.engine.Project
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
import org.cddcore.engine.Strings
import org.cddcore.engine.Reportable

@RunWith(classOf[JUnitRunner])
class ExampleWebsiteTest extends AbstractWebsiteTest with BeforeAndAfterAll {
  import Reportable._
  val project = Project("Example",
    CategorisePerson.categorise,
    ProcessChequeXml.processCheque,
    TennisScorer.scorer)

  val cddHandler = new CddHandler(project)
  val report = cddHandler.reportCreator.report
  val reportableToUrl = cddHandler.reportCreator.reportableToUrl

  val server = WebServer(cddHandler);

  def projectPageAtIndex = { go to (host); new ProjectPage(List(project, report)) }

  "The example website" should "display the project page if no page specified" in {
    projectPageAtIndex
  }

  it should "go to the project page if cdd logo clicked" in {
    projectPageAtIndex.clickLogo
  }

}