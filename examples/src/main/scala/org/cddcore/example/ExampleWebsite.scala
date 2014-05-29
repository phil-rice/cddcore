package org.cddcore.example

import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.example.tennisScore.TennisScorer
import org.cddcore.website.WebServer

object ExampleWebsite {

  def main(args: Array[String]) {
    WebServer(List(
      CategorisePerson.categorise,
      ProcessChequeXml.processCheque,
      TennisScorer.scorer)).launch
  }

}