package org.legacycdd.legacy

import org.cddcore.engine._
import scala.language.implicitConversions

case class LegacyData[ID, R](id: ID, params: List[Any], description: Option[String], actual: ROrException[R], expected: ROrException[R]) {
  def p0[P0] = params(0).asInstanceOf[P0]
  def p1[P1] = params(1).asInstanceOf[P1]
  def p2[P2] = params(2).asInstanceOf[P2]
  def fail = actual != expected
  def pass = actual == expected
}

class LegacyItem[ID, R](legacyData: LegacyData[ID, R], val categoriseConclusion: Conclusion, val replacementConclusion: Conclusion) extends LegacyData[ID, R](legacyData.id, legacyData.params, legacyData.description, legacyData.actual, legacyData.expected) with Reportable

class Legacy[ID, R](val idGen: Iterable[ID],
  idToParams: (ID) => List[Any],
  idToExpected: (ID) => ROrException[R],
  val replacement: EngineFull[R],
  val categorise: Engine1[LegacyData[ID, R], String],
  reporter: LegacyReporter[ID, R],
  idToDescription: (ID) => Option[String] = (id: ID) => None) {
  for (id <- idGen) {
    val params = idToParams(id)
    val expected = idToExpected(id)
    val replacementNode = replacement.findConclusionFor(params)
    val actual = replacement.evaluateConclusionNoException(params, replacementNode)
    val description = idToDescription(id)
    val legacyData = LegacyData(id, params, description, actual, expected)

    val categoriseParams = List(legacyData)
    val categoriseNode = categorise.findConclusionFor(categoriseParams)
    val categoriseActual = categorise.evaluateConclusionNoException(categoriseParams, categoriseNode)
    val item = new LegacyItem[ID, R](legacyData, categoriseNode, replacementNode)
    reporter.report(item)
  }
}