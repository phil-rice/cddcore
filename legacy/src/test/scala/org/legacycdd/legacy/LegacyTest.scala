package org.legacycdd.legacy

import org.junit.runner.RunWith
import 
 scala.language.implicitConversions
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

  implicit def toROrException(x: String) = ROrException[String](x)
  val categoriserEngine = Engine[LegacyData[Int, String], String]().
    useCase("Pass").expected("pass").
    scenario(LegacyData(1, List(1),None,  "X", "X")).because((l: LegacyData[Int, String]) => l.expected == l.actual).
    useCase("Fail").expected("fail").
    scenario(LegacyData(1, List(1), None, "X", "Y")).because((l: LegacyData[Int, String]) => l.expected != l.actual).
    build

  val passConclusion = categoriserEngine.root.right.get.yes.left.get
  val failConclusion = categoriserEngine.root.right.get.no.right.get.yes.left.get

  def makeLegacy(ids: Iterable[Int], reporter: LegacyReporter[Int, String]) =
    new Legacy[Int, String](ids,
      (x: Int) => List(x),
      (x: Int) => ROrException("X".padTo(x, "X").mkString),
      replacementEngine, categoriserEngine,
      reporter)

  "A Legacy" should "Execute ids and pass relevant parameters to the reporter" in {
    val reporter = new MemoryReporter[Int, String]
    val legacy = makeLegacy(List(1, 2, 3), reporter)


    assertEquals(List(2,1), reporter.itemsFor(passConclusion).map(_.id))
    assertEquals(List(3), reporter.itemsFor(failConclusion).map(_.id))
    assertEquals(List(3,1), reporter.itemsFor(conclusion1).map(_.id))
    assertEquals(List(2), reporter.itemsFor(conclusion2).map(_.id))
    
    assertEquals(List(failConclusion), reporter.itemsFor(failConclusion).map(_.categoriseConclusion))
    assertEquals(List(conclusion1), reporter.itemsFor(failConclusion).map(_.replacementConclusion))
  }

} 