package org.cddcore.example

import org.cddcore.engine.ByReferenceDocumentPrinterStrategy
import org.cddcore.engine.EngineWithLogger
import org.cddcore.engine.HtmlRenderer
import org.cddcore.engine.Report
import org.cddcore.engine.ReportableHolder
import org.cddcore.engine.ReportableToUrl
import org.cddcore.engine.SimpleKeyStrategy
import org.cddcore.engine.SimpleReportableToUrl
import org.cddcore.example.tennisScore.TennisScorer

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