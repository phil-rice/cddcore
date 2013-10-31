package org.legacycdd.legacy

import org.junit.runner.RunWith
import org.cddcore.engine._
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.Conclusion
import org.cddcore.engine.AbstractTest

@RunWith(classOf[JUnitRunner])
class LegacyTest extends AbstractTest {

  val replacementEngine = Engine[Int, String]().
    useCase("").
    scenario(1).expected("X").
    scenario(2).expected("XX").because((p: Int) => p == 2).
    build;

  val conclusion1 = replacementEngine.root.right.get.no.left.get
  val conclusion2 = replacementEngine.root.right.get.yes.left.get

  val categoriserEngine = Engine[String, String, String]().
    useCase("Pass").expected("pass").
    scenario("X", "X").because((l: String, r: String) => l == r).
    useCase("Fail").expected("fail").
    scenario("X", "Y").because((l: String, r: String) => l != r).
    build

  val passConclusion = categoriserEngine.root.right.get.yes.left.get
  val failConclusion = categoriserEngine.root.right.get.no.right.get.yes.left.get

  class LegacyReporterForTests extends LegacyReporter {
    var actualIds = List[Any]()
    var actualReplacement = List[Engine]()
    var actualCategoriser = List[Engine]()
    var actualReplacementConclusion = List[Conclusion]()
    var actualCategoriserConclusion = List[Conclusion]()

    def report[ID](id: ID, replacement: Engine, replacementConclusion: Conclusion, categorise: Engine, categoriseConclusion: Conclusion) {
      actualIds = id :: actualIds
      actualReplacement = replacement :: actualReplacement
      actualCategoriser = categorise :: actualCategoriser
      actualReplacementConclusion = replacementConclusion :: actualReplacementConclusion
      actualCategoriserConclusion = categoriseConclusion :: actualCategoriserConclusion
    }
  }

  def makeLegacy(ids: Iterable[Int], reporter: LegacyReporter) =
    new Legacy[Int, Int, String](ids,
      (x: Int) => List(x),
      (x: Int) => "X".padTo(x, "X").mkString,
      replacementEngine, categoriserEngine,
      reporter)

  "A Legacy" should "Execute ids and pass relevant parameters to the reporter" in {
    var reporter = new LegacyReporterForTests
    val legacy = makeLegacy(List(1, 2, 3), reporter)
    assertEquals(List(1, 2, 3), reporter.actualIds.reverse)
    assertEquals(List(replacementEngine, replacementEngine, replacementEngine), reporter.actualReplacement.reverse)
    assertEquals(List(conclusion1, conclusion2, conclusion1), reporter.actualReplacementConclusion.reverse)
    assertEquals(List(categoriserEngine, categoriserEngine, categoriserEngine), reporter.actualCategoriser.reverse)
    assertEquals(List(passConclusion, passConclusion, failConclusion), reporter.actualCategoriserConclusion.reverse)
  }

} 