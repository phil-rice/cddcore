package org.cddcore.example

import org.cddcore.engine.Project
import org.corecdd.website.WebServer
import org.corecdd.website.CddHandler
import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.example.tennisScore.TennisScorer

object ExampleWebsite {

  def main(args: Array[String]) {
    val project = Project("Example",
      CategorisePerson.categorise,
      ProcessChequeXml.processCheque,
      TennisScorer.scorer)
    WebServer(new CddHandler(project)).launch
  }

}