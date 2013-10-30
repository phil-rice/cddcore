package org.legacycdd.legacy

import org.cddcore.engine.Engine
import org.cddcore.engine.Conclusion

class Legacy[Id](val idGen: Iterable[Id],
  idToParams: (Id) => List[Any],
  idToExpected: (Id) => Any,
  replacement: Engine,
  categorise: Engine,
  report: (Id, Conclusion, Conclusion) => Unit) {
  for (id <- idGen) {
    val params = idToParams(id)
    val expected = idToExpected(id)
    val replacementNode = replacement.findConclusionFor(params)
    val actual = replacement.evaluateConclusion(params, replacementNode)

    val categoriseParams = List(id, expected, actual)
    val categoriseNode = categorise.findConclusionFor(categoriseParams)
    val categoriseActual = categorise.evaluateConclusion(categoriseParams, categoriseNode)
    report(id, replacementNode, categoriseNode)
  }

}