package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import scala.xml.NodeSeq
import Reportable._
import org.junit.Assert

trait ReportPages {
  object CddSection {
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
      case _ =>
        throw new IllegalArgumentException
    }
    def findReport(path: ReportableList) = reportPath(path).head.asInstanceOf[Report]
    def reportPath(path: ReportableList): ReportableList = path match {
      case (project: Report) :: tail => path
      case h :: tail => reportPath(tail)
      case _ => throw new IllegalArgumentException
    }

  }

  class CddSection(val path: ReportableList)
  class EmbeddedSection(path: ReportableList, node: Node) extends CddSection(path)

  class ReportSection(path: ReportableList, xml: Elem, reportableToUrl: ReportableToUrl) extends CddSection(path) with AssertEquals {
    import CddSection._
    type PageType <: ReportSection
    val report = findReport(path)
    val project = findProject(path)
    def asPageType = this.asInstanceOf[PageType]
    val divs = xml \\ "div"
    val reportDiv = onlyDivWith("report");

    val topLineDiv = onlyDivWith("topLine", reportDiv.child)
    val cddLogoDiv = onlyDivWith("cddLogo", topLineDiv.child)
    val cddBoxDiv = onlyDivWith("cddBox", topLineDiv.child)
    val reportTopBoxDiv = onlyDivWith("reportTopBox", topLineDiv.child)
    val reportTitlesDiv = divsWith("reportTitle", reportTopBoxDiv.child)
    assertTextEquals("Report name", reportTitlesDiv(0))
    assertTextEquals("Report date", reportTitlesDiv(1))
    val reportTextDiv = onlyDivWith("reportText", reportTopBoxDiv.child)
    val reportDateDiv = onlyDivWith("reportDate", reportTopBoxDiv.child)
    val engineDivs = divsWith("engine", reportDiv.child)
    val engines = project.engines.zip(engineDivs).map(_ match { case (e: Engine, node) => new EngineSection(e :: path, node) })
    assertEquals(project.engines.size, engines.size)

    class EngineSection(path: ReportableList, engineNode: Node) extends EmbeddedSection(path, engineNode) {
      val engine = path.head.asInstanceOf[Engine]
      val engineSummary = onlyDivWith("engineSummary", engineNode.child)
      val engineText = onlyDivWith("engineText", engineSummary.child)
      val engineTable = onlyDivWith("engineTable", engineSummary.child)
      val useCaseSummaries = divsWith("usecaseSummary", engineSummary.child)
      val decisionTree = onlyDivWith("decisionTree", engineNode.child)
      val usecaseSummarySections = engine.children.zip(useCaseSummaries).map(_ match { case (usecase: RequirementAndHolder, node) => new UseCaseSection(usecase :: path, node) })
      assertEquals(engine.children.size, usecaseSummarySections.size)
      
      val engineLink = (engineText \ "a").head
      assertEquals(reportableToUrl.url(path).get, engineLink.attribute("href").get.text) 
    }

    class UseCaseSection(path: ReportableList, useCaseNode: Node) extends EmbeddedSection(path, useCaseNode) {
      val useCase = path.head.asInstanceOf[RequirementAndHolder]
      val h4 = only(useCaseNode \ "h4") 
      val links = h4 \ "a"
      assertEquals(useCase.children.size + 1, links.size) //the +1 is the use case itself
      val useCaselink = links.head
      assertTextEquals(useCase.titleString, useCaselink)

      assertEquals(reportableToUrl.url(path).get, useCaselink.attribute("href").get text)
      val summaries = useCase.children.zip(links.tail).map(_ match { case (s: Test, node) => new SummarySection(s :: path, node) })

    }

    class SummarySection(path: ReportableList, summaryNode: Node) extends EmbeddedSection(path, summaryNode) {
      val test: Test = path.head.asInstanceOf[Test]
      assertEquals(reportableToUrl.url(path).get, summaryNode.attribute("href").get text)
    }

    def divsWith(className: String, divs: NodeSeq = divs) = divs filter attributeEquals("class", className)

    def onlyDivWith(className: String, divs: NodeSeq = divs) = only(divsWith(className, divs))
    def only(nodes: NodeSeq) = {
      assertEquals(1, nodes.size)
      nodes head
    }
    def attributeEquals(name: String, value: String)(node: Node) = {
      val n = node;
      val attribute = node.attribute(name)
      val filtered = attribute.filter(
        (v) =>
          v.toString == value)
      filtered.isDefined
    }
  }
}
@RunWith(classOf[JUnitRunner]) 
class ReportCreatorTests extends AbstractTest with ReportPages {

  val engine = Engine[Int, String]().title("EngTitle").
    useCase("uc1").
    scenario(10, "ten").expected("10->").
    useCase("uc2").
    scenario(20, "twenty").expected("20->").because((x: Int) => x == 20).
    scenario(21, "twentyone").expected("21->").because((x: Int) => x == 21).
    build
  val pForTitle = Project("ProjName", engine);
  "A ReportCreator" should "allow Html for a project to be produced" in {
    val rc = new ReportCreator(pForTitle, "ReportTitle", new SimpleReportableToUrl)
    val html = rc.htmlFor(List(pForTitle)).get
    val xml = XML.loadString(html)
    new ReportSection(List(pForTitle, rc.report), xml, rc.reportableToUrl)
  }
}