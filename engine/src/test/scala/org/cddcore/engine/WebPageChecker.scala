package org.cddcore.engine

import scala.xml.Elem
import scala.language.implicitConversions
import scala.xml.NodeSeq
import scala.xml.XML
import scala.xml.Node
import Reportable._
import scala.language.postfixOps

trait WebPageChecker extends AssertEquals {
  import PathUtils._
  def reportableToUrl: ReportableToUrl
  def report: Report
  def html: String

  lazy val urlMap: UrlMap = reportableToUrl.makeUrlMap(report)
  val xml: Elem = XML.loadString(html)
  val divs = xml \\ "div"
  val reportDiv = onlyDivWith("report");

  object CddSection {
    def findUseCase(path: ReportableList) = usecasePath(path).head.asInstanceOf[RequirementAndHolder]
    def usecasePath(path: ReportableList): ReportableList = path match {
      case (usecase: RequirementAndHolder) :: tail => path
      case h :: tail => usecasePath(tail)
      case _ => throw new IllegalArgumentException
    }

    def findEngine(path: ReportableList) = enginePath(path).head.asInstanceOf[EngineFull[_, _]]
    def enginePath(path: ReportableList): ReportableList = path match {
      case (engine: EngineFull[_, _]) :: tail => path
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

  class EngineSummaryChecker(path: ReportableList, engineNode: Node, withTest: Boolean) extends EmbeddedSection(path, engineNode) {
    import EngineWithLogger._
    val engine = findEngine(path)
    val loggerDisplayProcessor = engine.logger
    val engineSummary = onlyDivWith("engineSummary", engineNode.child)
    val engineText = onlyDivWith("engineText", engineSummary.child)
    val engineTable = onlyDivWith("engineTable", engineSummary.child)
    val decisionTree = onlyDivWith("decisionTree", engineNode.child)

    val engineLink = (engineText \ "a").head
    val engineLinkText = engineLink.text
    val engineLinkUrl = engineLink.attribute("href").get.text
    assertEquals(reportableToUrl.url(enginePath(path)).get, engineLinkUrl)

    if (withTest) {

    } else {
      val decisionTreeNode = onlyDivWith("decisionTree", engineNode.child)
      val decisionTreeChecker = new DecisionTreeSectionChecker(path, decisionTreeNode)
    }
  }

  class EngineSectionChecker(path: ReportableList, engineNode: Node, withTest: Boolean) extends EngineSummaryChecker(path, engineNode, withTest) {
    val useCaseSummaries = divsWith("usecaseSummary", engineSummary.child)

    def checkHasUseCaseSummarySections = {
      val usecaseSummarySections = engine.children.zip(useCaseSummaries).map(_ match { case (usecase: RequirementAndHolder, node) => new UseCaseSummarySectionChecker(usecase :: path, node) })
      assertEquals(engine.children.size, usecaseSummarySections.size)
    }

  }

  class SimpleUseCaseDetailSectionChecker(path: ReportableList, useCaseNode: Node) extends EmbeddedSection(path, useCaseNode) {
    val useCase = findUseCase(path)
    val useCasePath = findUseCasePath(path)
    val h4 = only(useCaseNode \ "h4")
    val links = h4 \ "a"
    assertEquals(1, links.size) //the +1 is the use case itself
    val useCaselink = links.head
    assertTextEquals(useCase.titleString, useCaselink)
    assertEquals(reportableToUrl.url(useCasePath).get, useCaselink.attribute("href").get text)

    val scenarios = divsWith("scenario", useCaseNode.child)

  }

  class UseCaseDetailSectionChecker(path: ReportableList, useCaseNode: Node) extends SimpleUseCaseDetailSectionChecker(path, useCaseNode) {
    val summaries = useCase.children.zip(scenarios).map(_ match { case (s: Test, node) => new ScenarioDetailSectionChecker(s :: useCasePath, node) })
    assertEquals(useCase.children.size, scenarios.size)
    assertEquals(useCase.children.size, summaries.size)
  }

  class OneUseCaseDetailSectionChecker(path: ReportableList, useCaseNode: Node) extends SimpleUseCaseDetailSectionChecker(path, useCaseNode) {
    new ScenarioDetailSectionChecker(path, scenarios(0))
    assertEquals(1, scenarios.size)
  }

  class ScenarioDetailSectionChecker(path: ReportableList, scenarioNode: Node) extends EmbeddedSection(path, scenarioNode) {
    import EngineWithLogger._
    val test: Test = path.head.asInstanceOf[Test]
    val engine = PathUtils.findEngine(path)
    val loggerDisplayProcessor = engine.logger
    val scenarioText = onlyDivWith("scenarioText", scenarioNode.child)
    val linkNode = only(scenarioText \ "a")
    val link = linkNode.attribute("href").get
    assertEquals(reportableToUrl.url(path).get, link text)
    val table = only(scenarioNode \ "table")
    val trs = table \ "tr"
    val paramTd = findTdInRowWithTitle(table, "Parameter")
    val paramText = paramTd.get.text
    for (p <- test.params) p match {
      case h: HtmlDisplay => // Don't know how to test this
      case _ => assert(paramText.contains(loggerDisplayProcessor(p)))
    }

    val expectedTd = findTdInRowWithTitle(table, "Expected")
    (expectedTd, test.expected) match {
      case (Some(td), Some(r: ROrException[_])) => assertTextEquals(r.toString, td);
      case _ => ;
    }
  }

  class ScenarioLinkChecker(scenario: Test, linkNode: Node) {

  }

  class UseCaseSummarySectionChecker(path: ReportableList, useCaseNode: Node) extends EmbeddedSection(path, useCaseNode) {
    val useCase = path.head.asInstanceOf[RequirementAndHolder]
    val h4 = only(useCaseNode \ "h4")
    val links = h4 \ "a"
    assertEquals(useCase.children.size + 1, links.size) //the +1 is the use case itself
    val useCaselink = links.head
    assertTextEquals(useCase.titleString, useCaselink)
    assertEquals(reportableToUrl.url(path).get, useCaselink.attribute("href").get text)

    val summaries = useCase.children.zip(links.tail).map(_ match { case (s: Test, node) => new ScenarioSummarySectionChecker(s, node) })
  }

  class ScenarioSummarySectionChecker(test: Test, summaryNode: Node) {
    assertEquals(urlMap.apply(test), summaryNode.attribute("href").get text)
  }

  object DecisionTreeSectionChecker {
    def apply(root: Either[Conclusion, Decision], path: ReportableList, node: Node, depth: Int) {
      root match {
        case Left(c) =>
          new ConclusionSectionChecker(c, path, node, depth);
        case Right(d) =>
          new DecisionSectionChecker(d, path, node, depth)
      }
    }
  }

  class DecisionTreeSectionChecker(path: ReportableList, decisionTreeNode: Node) extends EmbeddedSection(path, decisionTreeNode) {
    implicit def toEngine[R](e: Engine) = e.asInstanceOf[EngineBuiltFromTests[R]]
    val engine = findEngine(path)
    val root = engine.root
    val decisionDivs = divsWith("decision", decisionTreeNode.child)
    val checker = decisionDivs.size match {
      case 0 => None;
      case 1 => Some(DecisionTreeSectionChecker(root, path, decisionDivs head, 1))
      case x => throw new IllegalStateException(x.toString)
    }
  }

  class DecisionSectionChecker(val decision: Decision, path: ReportableList, node: Node, depth: Int) extends EmbeddedSection(path, node) {
    val nodesWithoutBlankText = removeBlankNodes(node)
    assertEquals(4, nodesWithoutBlankText.size)
    val becauseNode = nthNodeWith(nodesWithoutBlankText, 0, "because")
    val yesNode = nodesWithoutBlankText(1)
    val elseNode = nthNodeWith(nodesWithoutBlankText, 2, "else")
    val noNode = nodesWithoutBlankText(3)

    val because = new BecauseSectionChecker(decision, path, becauseNode, depth + 1)
    val yesChecker = DecisionTreeSectionChecker(decision.yes, path, yesNode, depth + 1)
    val noChecker = DecisionTreeSectionChecker(decision.no, path, noNode, depth)
  }
  class ConclusionSectionChecker(val conclusion: Conclusion, path: ReportableList, node: Node, depth: Int) extends EmbeddedSection(path, node) {
    val nodesWithoutBlankText = removeBlankNodes(node)
    val indent = onlyDivWith("indent", nodesWithoutBlankText)
    val keywordThen = nodesWithoutBlankText(1)
    val links = node \ "a"
    val scenarioCheckers = conclusion.scenarios.zip(links).map { case (t: Test, a: Node) => new ScenarioSummarySectionChecker(t, a) }
    assertEquals(conclusion.scenarios.size, scenarioCheckers.size)
  }

  class BecauseSectionChecker(val decision: Decision, path: ReportableList, node: Node, depth: Int) extends EmbeddedSection(path, node) {
    val indent = onlyDivWith("indent", node.child)
    val indentSize = indent.text.size
    val because = onlyDivWith("because", node.child)
    //TODO Don't know how to do this assertTextEquals("(" + decision.prettyString + ")", because)
  }

  def divsWith(className: String, divs: NodeSeq = divs) =
    divs filter attributeEquals("class", className)

  def nthNodeWith(parent: NodeSeq, n: Int, className: String) = {
    val result = parent(n)
    assertEquals(className, result.attribute("class").get.text)
    result
  }

  def onlyDivWith(className: String, divs: NodeSeq = divs) = only(divsWith(className, divs))
  def only(nodes: NodeSeq) = {
    assertEquals(1, nodes.size)
    nodes head
  }
  def removeBlankNodes(node: Node) =
    node.child.filter((n) => {
      val len = n.text.trim.length
      len != 0
    })
  def findTdInRowWithTitle(node: Node, title: String) = {
    val tr = findRowWithTitle(node, title)
    tr match {
      case Some(tr) =>
        val tds = tr \ "td"
        assertEquals(2, tds.size)
        val td = tds(1)
        assertEquals("value", td.attribute("class").get.text)
        Some(td)
      case None => None
    }
  }

  def findRowWithTitle(node: Node, title: String) = {
    (node \\ "tr").find((tr) => {
      val td = (tr \ "td")
      val tdWithTitle = td(0)
      val classAttribute = td.head.attribute("class")
      classAttribute match {
        case Some(ca) =>
          assertEquals(classAttribute.get.text, "title")
          tdWithTitle.text.trim == title.trim
        case _ => false
      }
    })
  }

  def attributeEquals(name: String, value: String)(node: Node) = {
    val n = node;
    val attribute = node.attribute(name)
    val filtered = attribute.filter(
      (v) =>
        v.toString == value)
    filtered.isDefined
  }

  class TopLineChecker(expectedTitle: String) {
    val topLineDiv = onlyDivWith("topLine", reportDiv.child)
    val cddLogoDiv = onlyDivWith("cddLogo", topLineDiv.child)
    val cddBoxDiv = onlyDivWith("cddBox", topLineDiv.child)
    val reportTopBoxDiv = onlyDivWith("reportTopBox", topLineDiv.child)
    val reportTitlesDiv = divsWith("reportTitle", reportTopBoxDiv.child)
    assertTextEquals("Report name", reportTitlesDiv(0))
    assertTextEquals("Report date", reportTitlesDiv(1))
    val reportTextDiv = onlyDivWith("reportText", reportTopBoxDiv.child)
    val reportDateDiv = onlyDivWith("reportDate", reportTopBoxDiv.child)
    assertTextEquals(expectedTitle.trim, reportTextDiv)
  }
}

class ProjectPageChecker(val path: ReportableList, val html: String, val reportableToUrl: ReportableToUrl) extends WebPageChecker {
  import PathUtils._
  val project = path(0).asInstanceOf[Project]
  val report = path(1).asInstanceOf[Report]
  new TopLineChecker("Project: " + project.titleString)
  val documentHolderDiv = onlyDivWith("documentHolder", divs)
  val documentAs = documentHolderDiv \\ "a"

  val engineHolderDiv = onlyDivWith("engineHolder", divs)
  val engineAs = engineHolderDiv \\ "a"
  assertEquals(project.engines.size, engineAs.size)
  val engines = project.engines.toList
  val urls = engines.map((e)=> reportableToUrl.url(e :: path).get).toSet
  val hrefs = engineAs.map((a) => a.attribute("href").get.text).toSet
  assertEquals(urls, hrefs)
}

class EnginePageChecker(val path: ReportableList, val html: String, val reportableToUrl: ReportableToUrl) extends WebPageChecker {
  val engine = path(0).asInstanceOf[Engine]
  val project = path(1).asInstanceOf[Project]
  val report = path(2).asInstanceOf[Report]
  new TopLineChecker("Engine: " + engine.titleOrDescription("<Unnamed>"))

  val engineDivs = divsWith("engineWithTests", reportDiv.child)
  assertEquals(1, engineDivs.size)
  new EngineSectionChecker(path, engineDivs.head, false).checkHasUseCaseSummarySections
}
class UseCasePageChecker(path: ReportableList, val html: String, val reportableToUrl: ReportableToUrl) extends WebPageChecker {
  val useCase = path(0).asInstanceOf[RequirementAndHolder];
  val engine = path(1).asInstanceOf[Engine]
  val project = path(2).asInstanceOf[Project]
  val report = path(3).asInstanceOf[Report]

  new TopLineChecker("Usecase: " + useCase.titleString)
  val engineDivs = divsWith("engineWithTests", reportDiv.child)
  assertEquals(1, engineDivs.size)
  val engineSummaryChecker = new EngineSummaryChecker(path.tail, engineDivs.head, false)
  val useCaseDiv = onlyDivWith("usecase", engineSummaryChecker.engineSummary.child)
  new UseCaseDetailSectionChecker(path, useCaseDiv)
}

class ScenarioPageChecker(path: ReportableList, val html: String, val reportableToUrl: ReportableToUrl) extends WebPageChecker {
  val scenario = path(0).asInstanceOf[Test];
  val useCase = path(1).asInstanceOf[RequirementAndHolder];
  val engine = path(2).asInstanceOf[EngineWithLogger]
  val project = path(3).asInstanceOf[Project]
  val report = path(4).asInstanceOf[Report]

  new TopLineChecker("Scenario: " + scenario.titleString)
  val engineDivs = divsWith("engineWithTests", reportDiv.child)
  assertEquals(1, engineDivs.size)
  val engineSummaryChecker = new EngineSummaryChecker(path.tail, engineDivs.head, true)
  val useCaseDiv = onlyDivWith("usecase", engineSummaryChecker.engineSummary.child)
  new OneUseCaseDetailSectionChecker(path, useCaseDiv)
}
