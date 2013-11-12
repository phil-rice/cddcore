package org.cddcore.example

import org.cddcore.example.tennisScore.TennisPathHandler
import org.cddcore.example.tennisScore.TennisScorer
import org.corecdd.website.WebServer
import org.corecdd.website.WebServer.defaultPort
import org.junit.runner.RunWith

object Example {

  def main(args: Array[String]) {
    import WebServer._
    val scorer = TennisScorer.scorer
    WebServer.withPreHandlers(defaultPort, TennisScorer.scorer, new TennisPathHandler(scorer)).launch
  }
} 