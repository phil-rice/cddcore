package org.legacycdd.legacy

import org.cddcore.engine._

import org.cddcore.engine.Maps

trait LegacyReporter[ID, R] {
  def report(id: ID, replacement: Engine[R], replacementConclusion: Conclusion, categorise: Engine[String], categoriseConclusion: Conclusion)
}

class MemoryReporter[ID, R] extends LegacyReporter[ID, R] {
  var replacementConclusionMap = Map[Engine[_], Map[Conclusion, List[ID]]]()
  var categoriseConclusionMap = Map[Engine[_], Map[Conclusion, List[ID]]]()

  def engines = categoriseConclusionMap.keySet.toList ::: replacementConclusionMap.keySet.toList

  def countFor(e: Engine[_], c: Conclusion) = {
    val map = if (replacementConclusionMap.contains(e)) replacementConclusionMap else if (categoriseConclusionMap.contains(e)) categoriseConclusionMap else
      throw new IllegalArgumentException
    map.getOrElse(e, Map()).getOrElse(c, Nil).size
  }
  def report(id: ID, replacement: Engine[R], replacementConclusion: Conclusion, categorise: Engine[String], categoriseConclusion: Conclusion) {
    import Maps._

    replacementConclusionMap = Maps.addToMapOfMapOfList(replacementConclusionMap, replacement, replacementConclusion, id)
    categoriseConclusionMap = Maps.addToMapOfMapOfList(categoriseConclusionMap, categorise, categoriseConclusion, id)

  }

}