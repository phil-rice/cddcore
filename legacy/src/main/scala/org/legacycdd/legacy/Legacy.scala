package org.legacycdd.legacy

import org.cddcore.engine._

object Legacy {

}

class Legacy[ID, R](val idGen: Iterable[ID],
  idToParams: (ID) => List[Any],
  idToExpected: (ID) => ROrException[R],
  val replacement: Engine[R],
  val categorise: Engine2[ROrException[R], ROrException[R], String],
  reporter: LegacyReporter[ID,R]) {
  for (id <- idGen) {
    val params = idToParams(id)
    val expected = idToExpected(id)
    val replacementNode = replacement.findConclusionFor(params)
    val actual = replacement.evaluateConclusionNoException(params, replacementNode)

    val categoriseParams = List(expected, actual)
    val categoriseNode = categorise.findConclusionFor(categoriseParams)
    val categoriseActual = categorise.evaluateConclusionNoException(categoriseParams, categoriseNode)
    reporter.report(id, replacement, replacementNode, categorise, categoriseNode)
  }
}