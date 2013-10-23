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

object Reports {

  def main(args: Array[String]) {
    val project = Project("Example",
      CategorisePerson.categorise,
      ProcessCheque.processCheque,
      ProcessChequeXml.processCheque,
      TennisScorer.scorer)
    val e = TennisScorer.scorer.references;
    val html = Report("Static",  project).html
    val file = new File("C:/Users/Phil/Desktop/stuff.html")
    file.delete()
    Files.appendToFile(file)((p) =>
      //      p.append(html)
      p.append(TennisScorer.scorer.toStringWith(new HtmlIfThenPrinter)))
    println(TennisScorer.scorer.toStringWith(new HtmlIfThenPrinter))
  }
}