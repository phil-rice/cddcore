package org.cddcore.example

import org.cddcore.engine.reporting.ReportCreator
import org.cddcore.engine.Project
import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.engine.LoggerDisplayProcessor
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.example.tennisScore.TennisScorer


object Reports {

  def main(args: Array[String]) {
    val project = Project("Example",
      CategorisePerson.categorise,
      ProcessChequeXml.processCheque,
      TennisScorer.scorer)
    ReportCreator.fileSystem(LoggerDisplayProcessor(), project).create
    println("done")
  }
}