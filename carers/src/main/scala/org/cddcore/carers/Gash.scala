package org.cddcore.carers

import scala.concurrent.stm._
import scala.xml._
import org.joda.time._
import org.joda.time.format._


object Gash {
  def main(args: Array[String]) {
    println("Sunday: " + Xmls.asDate("2013-9-8").dayOfWeek().get())
    println("Monday: " + Xmls.asDate("2013-9-9").dayOfWeek().get())
  }
}