package org.cddcore.legacy

import org.cddcore.engine._
import scala.language.implicitConversions
import org.cddcore.engine.builder._
import org.cddcore.utilities.Exceptions
import org.cddcore.utilities.Maps
import org.cddcore.utilities.Orderings

case class LegacyData[ID, Params, R](id: ID, params: Params, description: Option[String], actual: Either[Exception, R], expected: Either[Exception, R]) {
  def fail = actual != expected
  def pass = actual == expected
}

trait AnyLegacyItem{
  def toLegacyItem[ID, Params, R] = asInstanceOf[LegacyItem[ID, Params, R]]
  def toId[ID]: Any = toLegacyItem[ID, Any, Any].id
  def toDescription: Option[String] = toLegacyItem[Any, Any, Any].description
  def toExpected[R]: Either[Exception, R] = toLegacyItem[Any, Any, R].expected
  def toActual[R]: Either[Exception, R] = toLegacyItem[Any, Any, R].actual
}
class LegacyItem[ID, Params, R](legacyData: LegacyData[ID, Params, R], val categoriseConclusion: AnyConclusion, val replacementConclusion: AnyConclusion, val textOrder: Int = Reportable.nextTextOrder) extends LegacyData[ID, Params, R](legacyData.id, legacyData.params, legacyData.description, legacyData.actual, legacyData.expected) with Reportable with AnyLegacyItem

trait Legacy[ID, Params, R, E <: Engine] {
  def replacement: AnyEngine[Params, R]
  def categorise: AnySingleConclusionEngine[LegacyData[ID, Params, R], String]
}

class SingleConclusionLegacy[ID, Params, R](val idGen: Iterable[ID],
  idToParams: (ID) => Params,
  idToExpected: (ID) => Either[Exception, R],
  val replacement: AnySingleConclusionEngine[Params, R],
  val categorise: AnySingleConclusionEngine[LegacyData[ID, Params, R], String],
  reporter: LegacyMonitor[ID, Params, R],
  idToDescription: (ID) => Option[String] = (id: ID) => None)  extends Legacy[ID, Params, R, AnySingleConclusionEngine[Params, R]]{

  for (id <- idGen) {
    val params = idToParams(id)
    val expected = idToExpected(id)
    val replacementConclusion = replacement.findConclusionFor(params)
    val actual = Exceptions(replacement.evaluateConclusion(replacementConclusion, params))
    val description = idToDescription(id)
    val legacyData = LegacyData(id, params, description, actual, expected)

    val categoriseConclusion = categorise.findConclusionFor(legacyData)
    val categoriseActual = Exceptions(categorise.evaluateConclusion(categoriseConclusion, legacyData))
    val item = new LegacyItem[ID, Params, R](legacyData, categoriseConclusion, replacementConclusion)
    reporter.report(item)
  }
}

trait LegacyMonitor[ID,Params, R] {
  type L = LegacyItem[ID, Params, R]
  def report(item: L)
}

class MemoryLegacyMonitor[ID, Params, R] extends LegacyMonitor[ID, Params, R] {
  var idToItem = Map[ID, L]()
  var conclusionToItems = Map[AnyConclusion, List[L]]()
  lazy val items = idToItem.keys.toList.sorted(Orderings.natural).map(idToItem)

  def itemsFor(c: AnyConclusion) = conclusionToItems.getOrElse(c, Nil)

  def report(item: LegacyItem[ID,Params, R]) {
    import Maps._
    idToItem = idToItem + (item.id -> item)
    conclusionToItems = Maps.addToList(conclusionToItems, item.replacementConclusion, item)
    conclusionToItems = Maps.addToList(conclusionToItems, item.categoriseConclusion, item)
  }

}
