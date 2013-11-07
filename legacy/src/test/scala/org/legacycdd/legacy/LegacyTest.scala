package org.legacycdd.legacy

import org.junit.runner.RunWith
import org.cddcore.engine._
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.Conclusion
import org.cddcore.engine.AbstractTest

@RunWith(classOf[JUnitRunner])
class LegacyTest extends AbstractTest  {

  val replacementEngine = Engine[Int, String]().
    useCase("").
    scenario(1).expected("X").
    scenario(2).expected("XX").because((p: Int) => p == 2).
    build;

  val conclusion1 = replacementEngine.root.right.get.no.left.get
  val conclusion2 = replacementEngine.root.right.get.yes.left.get

  val categoriserEngine = Engine[ROrException[String], ROrException[String], String]().
    useCase("Pass").expected("pass").
    scenario(ROrException("X"), ROrException("X")).because((l: ROrException[String], r:  ROrException[String]) => l == r).
    useCase("Fail").expected("fail").
    scenario(ROrException("X"), ROrException("Y")).because((l:  ROrException[String], r:  ROrException[String]) => l != r).
    build
    
  val passConclusion = categoriserEngine.root.right.get.yes.left.get
  val failConclusion = categoriserEngine.root.right.get.no.right.get.yes.left.get

  class LegacyReporterForTests [ID, R]extends LegacyReporter[ID, R] {
    var actualIds = List[ID]()
    var actualReplacement = List[Engine[R]]()
    var actualCategoriser = List[Engine[String]]()
    var actualReplacementConclusion = List[Conclusion]()
    var actualCategoriserConclusion = List[Conclusion]()

    def report(id: ID, replacement: Engine[R], replacementConclusion: Conclusion, categorise: Engine[String], categoriseConclusion: Conclusion) {
      actualIds = id :: actualIds
      actualReplacement = replacement :: actualReplacement
      actualCategoriser = categorise :: actualCategoriser
      actualReplacementConclusion = replacementConclusion :: actualReplacementConclusion
      actualCategoriserConclusion = categoriseConclusion :: actualCategoriserConclusion
    }
  }

  def makeLegacy(ids: Iterable[Int], reporter: LegacyReporter[Int,String]) =
    new Legacy[Int, String](ids,
      (x: Int) => List(x),
      (x: Int) => ROrException("X".padTo(x, "X").mkString),
      replacementEngine, categoriserEngine,
      reporter)

  "A Legacy" should "Execute ids and pass relevant parameters to the reporter" in {
    var reporter = new LegacyReporterForTests[Int, String]
    val legacy = makeLegacy(List(1, 2, 3), reporter)
    assertEquals(List(1, 2, 3), reporter.actualIds.reverse)
    assertEquals(List(replacementEngine, replacementEngine, replacementEngine), reporter.actualReplacement.reverse)
    assertEquals(List(conclusion1, conclusion2, conclusion1), reporter.actualReplacementConclusion.reverse)
    assertEquals(List(categoriserEngine, categoriserEngine, categoriserEngine), reporter.actualCategoriser.reverse)
    assertEquals(List(passConclusion, passConclusion, failConclusion), reporter.actualCategoriserConclusion.reverse)
  }

} 