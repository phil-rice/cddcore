package org.cddcore.example

import org.corecdd.website.WebServer

object Example {

  def main(args: Array[String]) {
    WebServer(8080, "org.cddcore.example.tennisScore", //http://localhost:8080/tennis
      "org.cddcore.example.customerCategorisation"). //http://localhost:8080/person
      launch
  } 

} 