package org.cddcore.htmlRendering

import org.cddcore.utilities._
import org.cddcore.engine._

case class DocumentHolder(val nodes: List[Document], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable
case class EngineHolder(val engines: List[Engine], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable {
  import EngineTools._
  val nodes = engines.map(_.asRequirement)
}