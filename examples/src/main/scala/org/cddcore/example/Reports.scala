package org.cddcore.example

import org.cddcore.engine.Report
import org.cddcore.example.tennisScore.TennisScorer
import org.cddcore.engine.Project
import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.example.processCheque_DM_2.ProcessCheque
import org.cddcore.engine.Files
import java.io.File
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.engine.DefaultIfThenPrinter
import org.cddcore.engine.HtmlIfThenPrinter
import org.cddcore.engine.ReportCreator

object Reports {

  def main(args: Array[String]) {
    val project = Project("Example",
      CategorisePerson.categorise,
      ProcessChequeXml.processCheque,
      TennisScorer.scorer)
    ReportCreator.fileSystem(project).create
    println("done")
  }
}