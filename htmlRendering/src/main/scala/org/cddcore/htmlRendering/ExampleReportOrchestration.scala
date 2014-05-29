package org.cddcore.htmlRendering

import java.util.Date
import org.cddcore.engine._
import org.cddcore.utilities._
import StartChildEndType._
import EngineTools._

object ExampleReportOrchestration {
  def main(args: Array[String]) {
    import SampleContexts._
    
    new ReportOrchestrator("file:///c:/users/phil/.cdd2", "title", List(eWithUsecasesAndScenarios, folding,eBlankTitleDoc1), new Date()).makeReports
  }
}