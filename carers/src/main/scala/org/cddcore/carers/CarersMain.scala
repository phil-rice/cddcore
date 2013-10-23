package org.cddcore.carers

import scala.language.implicitConversions
import org.cddcore.engine._

object CarersMain {
  def main(args: Array[String]) {
      val printer = RequirementsPrinter.html
      val result = Carers.engine.fold(ResultAndIndent())(printer)
      println(result)
  }
}