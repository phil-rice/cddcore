package org.cddcore.htmlRendering

import org.cddcore.engine.AbstractTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import SampleContexts._
import java.util.Date
import org.scalatest.BeforeAndAfterAll
import org.cddcore.utilities.Strings
import Strings.url

@RunWith(classOf[JUnitRunner])
class ReportOrchestrationTest extends AbstractTest {
  val writer = new MemoryReportWriter
  val rootUrl = "file:/c:/users/xx/.cdd2"
  val title = "reportTitle"
  val projectRoot = url(rootUrl, title)
  val orchestrator = new ReportOrchestrator(rootUrl, title, List(eBlankTitleDoc1, eWithUsecasesAndScenarios, folding), new Date(), writer)

  "A ReportOrchestrator" should "print to files for the root report, documents, the engines, usecases and scenarios" in {
    orchestrator.makeReports
    val map = writer.map
    val urls = map.keys.toList.sortBy((x) => x)
    val expected = List(
      "file:/c:/users/xx/.cdd2/reportTitle/index.html",
      "file:/c:/users/xx/.cdd2/reportTitle/EBlankTitle.EngineDescription.html",
      "file:/c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios.EngineDescription.html",
      "file:/c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase0.UseCase.html",
      s"file:/c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase0/Scenario${uc0s0.textOrder}.Scenario.html",
      "file:/c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase1.UseCase.html",
      s"file:/c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s1.textOrder}.Scenario.html",
      s"file:/c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s2.textOrder}.Scenario.html",
      "file:/c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title.FoldingEngineDescription.html",
      "file:/c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce0.EngineDescription.html",
      s"file:/c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce0/Scenario${ce0s0.textOrder}.Scenario.html",
      "file:/c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce1.EngineDescription.html",
      s"file:/c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce1/Scenario${ce1s1.textOrder}.Scenario.html",
      s"file:/c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce1/Scenario${ce1s2.textOrder}.Scenario.html").sortBy((x) => x)
    //    for ((e, a) <- expected.zipAll(urls, null, null))
    //      assertEquals(e, a)
    assertEquals(expected, urls)
  }

  it should "should find the pathToConclusion in a scenario with an engine from tests" in {
    import SampleContexts._

    val path = List(uc0s0, uc0, eWithUsecasesAndScenariosEd, engineReport)
    val conclusionPath = orchestrator.pathToConclusion(path)
    assertEquals(List(conclusionNo, decision), conclusionPath)

  }
  it should "should find the pathToConclusion in a scenario with a folding engine" in {
    import SampleContexts._

    val path = List(ce1s1, ce1ED, foldingED, foldingEngineAndDocumentReport)
    val conclusionPath = orchestrator.pathToConclusion(path)

    assertEquals(List(concYesCe1, decisionCe1), conclusionPath)
  }

  it should "return an empty list from pathToConclusion if the path head isn't a scenario" in {
    import SampleContexts._
    val path = List(uc0, eWithUsecasesAndScenariosEd, engineReport)
    val conclusionPath = orchestrator.pathToConclusion(path)
    assertEquals(List(), conclusionPath)
  }

}