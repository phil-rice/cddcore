package org.cddcore.htmlRendering

import scala.language.implicitConversions

import org.cddcore.engine._
import org.cddcore.engine.builder.Decision
import org.cddcore.engine.builder.DecisionTreeNode
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ReportableHelper._
import org.cddcore.utilities._
import SampleContexts._
import StartChildEndType._
import java.util.Date
import org.cddcore.engine.builder._
import ReportableHelper._

@RunWith(classOf[JUnitRunner])
class RenderedPageIntegrationTests extends AbstractTest {

  "A rendered report" should "pass the top line check" in {
    def checkTopLine(r: Report) {
      val htmlAndContext = Report.htmlAndRenderedContext(r)
      val checker = new SimpleHtmlRenderedChecker(htmlAndContext)
      checker.checkTopLine(s"Project: ${r.titleString}")
    }
    checkTopLine(eBlankTitleReport)
    checkTopLine(engineReport)
    checkTopLine(foldingEngineAndDocumentReport)
    checkTopLine(eBlankTitleDoc1_DocAndEngineReport)
    //    checkTopLine(eBlankReport) 
  }

  "A rendered focus report" should "pass the engine section check for reports with one engine" in {
    def checkReportWithOneEngine(r: Report, ed: EngineRequirement[_, _, _, _]) {
      val htmlAndContext = Report.htmlAndRenderedContext(r)
      val checker = new SimpleHtmlRenderedChecker(htmlAndContext)
      val engineWithTestsDiv = checker.onlyDivWith("engineWithTests")
      checker.checkEngineSummary(List(ed, engineReport), engineWithTestsDiv)
    }
    checkReportWithOneEngine(engineReport, eWithUsecasesAndScenariosEd)
    checkReportWithOneEngine(eBlankTitleReport, eBlankTitleED)
  }

  it should "pass the engine section test when the focus is lower down" in {
    def checkReport(e: Engine, focusPathReversed: Reportable*) {
      import EngineTools._
      val ed = e.asRequirement
      val focusPath = focusPathReversed.reverse.toList :+ ed
      val report = Report.focusedReport(Some("sort of title"), focusPath)
      val rc = RenderContext(UrlMap() ++ ed.pathsIncludingTreeAndEngine(List(report)), new Date, "")
      val html = Report.html(report, HtmlRenderer.engineReportSingleItemRenderer, rc)
      val checker = new SimpleHtmlRenderedChecker(html, rc)
      val engineWithTestsDiv = checker.onlyDivWith("engineWithTests")
      checker.checkEngineSummary(List(ed, engineReport), engineWithTestsDiv)
    }
    checkReport(eWithUsecasesAndScenarios) 
    checkReport(eWithUsecasesAndScenarios, uc0)
    checkReport(eWithUsecasesAndScenarios, uc0, uc0s0)
    checkReport(eWithUsecasesAndScenarios, uc1, uc1s1)
    checkReport(eWithUsecasesAndScenarios, uc1, uc1s2)
    checkReport(eWithUsecasesAndScenarios, uc1, uc1s2)
  }
  it should "pass the engine section test when the focus is lower down for folding engines" in {
    def checkReport(e: FoldingEngine[_, _, _, _, _], expectedChildEngines: List[Engine], focusPathReversed: Reportable*) {
      import EngineTools._
      val fed = e.asRequirement
      val focusPath = focusPathReversed.reverse.toList :+ fed
      val report = Report.focusedReport(Some("sort of title"), focusPath)
      val rc = RenderContext(UrlMap() ++ fed.pathsIncludingTreeAndEngine(List(report)), new Date, "")
      val html = Report.html(report, HtmlRenderer.engineReportSingleItemRenderer, rc)
      val checker = new SimpleHtmlRenderedChecker(html, rc)
      val childEngineDivs = checker.divsWith("childEngine")
      assertEquals(expectedChildEngines.size, childEngineDivs.size)
      for ((ce, div) <- expectedChildEngines.zip(childEngineDivs))
        checker.checkEngineSummary(List(ce.asRequirement, fed, engineReport), div)
    }
    checkReport(folding, folding.engines)
    checkReport(folding, List(folding.engines(0)), ce0ED)
    checkReport(folding, List(folding.engines(0)), ce0ED, ce0s0)
    checkReport(folding, List(folding.engines(1)), ce1ED)
    checkReport(folding, List(folding.engines(1)), ce1ED, ce1s1)
  }
  it should "pass the usecase section test when scenarios are just icons" in {
    def checkReport(e: Engine, expectedUsecases: List[UseCase[_, _, _, _]], focusPathReversed: Reportable*) {
      import EngineTools._
      val focusPath = focusPathReversed.reverse.toList :+ e.asRequirement
      val report = Report.focusedReport(Some("sort of title"), focusPath)
      val rc = RenderContext(UrlMap() ++ e.asRequirement.pathsIncludingTreeAndEngine(List(report)), new Date, "")
      val html = Report.html(report, HtmlRenderer.engineReportSingleItemRenderer, rc)
      val checker = new SimpleHtmlRenderedChecker(html, rc)

      val usecaseDivs = checker.divsWith("usecaseSummary")
      for ((uc, div) <- expectedUsecases.zipAll(usecaseDivs, null, null))
        checker.checkUsecaseWithScenariosSummarized(List(uc), div)
    }
    checkReport(eWithUsecasesAndScenarios, List(uc0, uc1))
  }
  it should "pass the usecase section test when scenarios are just details in usecases" in {
    def checkReport(e: Engine, expectedUsecases: List[UseCase[_, _, _, _]], focusPathReversed: Reportable*) {
      import EngineTools._
      val focusPath = focusPathReversed.reverse.toList :+ e.asRequirement
      val report = Report.focusedReport(Some("sort of title"),  focusPath)
      val rc = RenderContext(UrlMap() ++ e.asRequirement.pathsIncludingTreeAndEngine(List(report)), new Date, "")
      val html = Report.html(report, HtmlRenderer.useCaseOrScenarioReportRenderer, rc)
      val checker = new SimpleHtmlRenderedChecker(html, rc)

      val usecaseDivs = checker.divsWith("usecaseSummary")
      for ((uc, div) <- expectedUsecases.zipAll(usecaseDivs, null, null))
        checker.checkUsecaseWithScenariosDetails(List(uc), div)
    }
    checkReport(eWithUsecasesAndScenarios, List(uc0, uc1))
    checkReport(eWithUsecasesAndScenarios, List(uc0), uc0)
    checkReport(eWithUsecasesAndScenarios, List(uc1), uc1)
  }
  it should "pass the scenario section test " in {
    def checkReport(e: Engine, expectedScenarios: List[Scenario[_, _, _, _]], focusPathReversed: Reportable*) {
      import EngineTools._
      val focusPath = focusPathReversed.reverse.toList :+ e.asRequirement
      val report = Report.focusedReport(Some("sort of title"), focusPath)
      val rc = RenderContext(UrlMap() ++ e.asRequirement.pathsIncludingTreeAndEngine(List(report)), new Date, "")
      //      val classPaths = report.reportPaths.map(_.map(_.getClass().getSimpleName()))
      //      val paths = Lists.pathToStartChildEnd(classPaths)
      //      println(paths.mkString("\n"))
      val html = Report.html(report, HtmlRenderer.useCaseOrScenarioReportRenderer, rc)
      val checker = new SimpleHtmlRenderedChecker(html, rc)

      val scenarioDivs = checker.divsWith("scenario")
      for ((s, div) <- expectedScenarios.zipAll(scenarioDivs, null, null))
        checker.checkScenarioDetails(List(s), div)
    }
    checkReport(eWithUsecasesAndScenarios, List(uc0s0, uc1s1, uc1s2))
    checkReport(eWithUsecasesAndScenarios, List(uc0s0), uc0)
    checkReport(eWithUsecasesAndScenarios, List(uc1s1), uc1, uc1s1)
  }

}