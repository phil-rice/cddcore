package org.legacycdd.legacy

import org.cddcore.engine._

import org.cddcore.utilities.Maps

trait LegacyReporter[ID, R] {
  type L = LegacyItem[ID, R]
  def report(item: L)
}

class MemoryReporter[ID, R] extends LegacyReporter[ID, R] {
  var idToItem = Map[ID, L]()
  var conclusionToItems = Map[Conclusion, List[L]]()

  def itemsFor(c: Conclusion) = conclusionToItems.getOrElse(c, Nil)

  def report(item: LegacyItem[ID, R]) {
    import Maps._
    idToItem = idToItem + (item.id -> item)
    conclusionToItems = Maps.addToList(conclusionToItems, item.replacementConclusion, item)
    conclusionToItems = Maps.addToList(conclusionToItems, item.categoriseConclusion, item)
  }

}