package org.cddcore.example

import org.cddcore.engine.ByReferenceDocumentPrinterStrategy
import org.cddcore.engine.SimpleKeyStrategy
import org.cddcore.engine.Report
import org.cddcore.example.tennisScore.TennisScorer
import org.cddcore.engine.HtmlRenderer
import org.cddcore.engine.EngineWithLogger
import org.cddcore.engine.ReportableToUrl
import org.cddcore.engine.SimpleReportableToUrl
import org.cddcore.engine.ReportableHolder

object DocumentExample {

  def main(args: Array[String]) {
    val engine = TennisScorer.scorer.asInstanceOf[EngineWithLogger]
    val rawReport = Report("Tennis Document", engine)
    val report = new ByReferenceDocumentPrinterStrategy(None, new SimpleKeyStrategy).makeReportOfJustDocuments(rawReport).asInstanceOf[ReportableHolder]
    val reportableToUrl = new SimpleReportableToUrl
    val urlMap = reportableToUrl.makeUrlMap(rawReport)
    val html = new HtmlRenderer(engine.logger, false).documentsHtml(None).
      render(reportableToUrl, urlMap, report)
    println(html)
  }

}