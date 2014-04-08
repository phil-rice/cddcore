package org.cddcore.example

import org.cddcore.engine._
import org.cddcore.example.tennisScore.TennisScorer
import org.cddcore.engine.reporting._
import org.cddcore.engine.utilities.SimpleKeyStrategy

object DocumentExample {

  def main(args: Array[String]) {
    val engine = TennisScorer.scorer.asInstanceOf[EngineWithLogger]
    val rawReport = Report("Tennis Document", engine)
    val strategy = new ByReferenceDocumentPrinterStrategy(None, new SimpleKeyStrategy)
    val documentDetails = strategy.makeReportOfJustDocuments(engine)
    val report = Report("Some report",  documentDetails)
    val reportableToUrl = new SimpleReportableToUrl
    val urlMap = reportableToUrl.makeUrlMap(rawReport)
    val html = new HtmlRenderer(engine.logger, false).documentsHtml(None).
      render(reportableToUrl, urlMap, report)
    println(html) 
  }

}