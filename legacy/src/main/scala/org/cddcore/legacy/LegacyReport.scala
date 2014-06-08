package org.cddcore.legacy

import org.cddcore.engine._
import org.cddcore.utilities.Maps
import org.cddcore.engine.builder.Conclusion
import org.cddcore.engine.builder.AnyConclusion
import org.cddcore.htmlRendering.Report
import org.cddcore.utilities.Orderings
import org.cddcore.utilities.Orderings

object LegacyReport {
  def defaultItemsToDisplay = 10
  def apply[ID, Params, R](title: String, legacy: Legacy[ID, Params, R, AnySingleConclusionEngine[Params, R]], monitor: MemoryLegacyMonitor[ID, Params, R], conclusion: Option[AnyConclusion] = None, itemstoDisplay: Int = defaultItemsToDisplay): SingleConclusionLegacyReport[ID, Params, R] =
    new SingleConclusionLegacyReport[ID, Params, R](Some(title), legacy, monitor, itemstoDisplay = itemstoDisplay, conclusion = conclusion)
}

case class ConclusionAsTitle(conclusion: AnyConclusion) extends Reportable {
  val textOrder = conclusion.textOrder
}

case class SingleConclusionLegacyReport[ID, Params, R](title: Option[String], legacy: Legacy[ID, Params, R, AnySingleConclusionEngine[Params, R]], monitor: MemoryLegacyMonitor[ID, Params, R], conclusion: Option[AnyConclusion] = None, description: Option[String] = None, itemstoDisplay: Int = LegacyReport.defaultItemsToDisplay, textOrder: Int = Reportable.nextTextOrder) extends Report {
  private val idToItems = monitor.idToItem
  private val conclusionToItems = monitor.conclusionToItems
  private def items(c: AnyConclusion) = conclusionToItems(c).sortBy(_.id)(Orderings.natural).take(itemstoDisplay)
  private val validItems = conclusion match {
    case Some(c) => conclusionToItems(c).toSet
    case _ => Set[LegacyItem[ID, Params, R]]()
  }
  private val validItemFilter = conclusion match {
    case Some(c) => (li: LegacyItem[ID, Params, R]) => validItems.contains(li)
    case None => (li: LegacyItem[ID, Params, R]) => true
  }
  private def pathFor(e: Engine): List[List[Reportable]] = {
    import EngineTools._
    val ed = e.asRequirement
    val conclusionsInEngine = e.trees.flatMap(_.root.conclusions)
    val conclusions = conclusion match { case Some(c) if conclusionsInEngine.contains(c) => List(c); case _ => e.trees.flatMap(_.root.conclusions) }
    val tail: List[List[Reportable]] = conclusions.flatMap((c) => {
      val h: List[Reportable] = List(ConclusionAsTitle(c), ed, this)
      val m: List[List[Reportable]] = (conclusionToItems.get(c) match {
        case Some(list) => {
          val validItems = list.filter(validItemFilter);
          validItems.sortBy(_.id)(Orderings.natural).take(itemstoDisplay).map((i) => List(i, ConclusionAsTitle(c), ed, this))
        }
        case None => List()
      })
      val t: List[List[Reportable]] = ??? //e.trees.map(_.treePathsWithElseClause(List(ed, this)))
      m match {
        case Nil => Nil
        case _ => h :: m :: t
      }
    })
    List[Reportable](ed, this) :: tail
  }
  def reportPaths: List[List[org.cddcore.engine.Reportable]] =
    List(this) ::
      (pathFor(legacy.categorise) :::
        pathFor(legacy.replacement))
}


