package org.legacycdd.legacy

import org.cddcore.engine._

trait LegacyReporter {
  def report[ID](id: ID, replacement: Engine, replacementConclusion: Conclusion, categorise: Engine, categoriseConclusion: Conclusion)
}

class Legacy[Id, P, R](val idGen: Iterable[Id],
  idToParams: (Id) => List[Any],
  idToExpected: (Id) => R,
  val replacement: Engine,
  val categorise: Engine,
  reporter: LegacyReporter) {
  for (id <- idGen) {
    val params = idToParams(id)
    val expected = idToExpected(id)
    val replacementNode = replacement.findConclusionFor(params)
    val actual = replacement.evaluateConclusion(params, replacementNode)

    val categoriseParams = List(expected, actual)
    val categoriseNode = categorise.findConclusionFor(categoriseParams)
    val categoriseActual = categorise.evaluateConclusion(categoriseParams, categoriseNode)
    reporter.report(id, replacement, replacementNode, categorise, categoriseNode)
  }

}
