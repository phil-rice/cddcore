package org.cddcore.engine

import org.joda.time.DateTime

case class Report(reportTitle: String, date: String, requirements: Requirement*) extends RequirementHolder {
  val title = Some(reportTitle)
  val children = requirements.toList
  def description = None
  def priority = 0
  def references = List[Reference]()
  def html = fold(ResultAndIndent())(RequirementsPrinter.html).result
}