package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ByReferenceDocumentPrinterStrategyTest extends AbstractTest {
  import Key._
  implicit def reqToRequirementAndNone(r: RequirementAndHolder) = new RequirementAndEngine(r, None)
  implicit def tupleToRequirementAndSomeEngine(t: (RequirementAndHolder, Engine)) = new RequirementAndEngine(t._1, Some(t._2))

  val eMain = Engine[Int, String]().title("Main Engine").
    useCase("UC 1", "The body of the first use case").priority(3).
    scenario(11).expected("one-one").because((x: Int) => x == 11).
    scenario(12).expected("one-two").because((x: Int) => x == 12).reference("7.3", None).
    scenario(13).expected("one-three").because((x: Int) => x == 13).

    useCase("UC 2", "The body of the second use case").
    scenario(21).expected("two-one").because((x: Int) => x == 21).
    scenario(22).expected("two-two").because((x: Int) => x == 22).

    useCase("UC 3", "The body of the third use case").
    scenario(31).expected("three-one").because((x: Int) => x == 31).
    scenario(32).expected("three-two").because((x: Int) => x == 32).
    build

  val useCases = eMain.all(classOf[UseCase])
  val scenarios = eMain.all(classOf[Test])
  val strategy = new ByReferenceDocumentPrinterStrategy(None, new SimpleKeyStrategy, debug = true)
  val project = Project("Some title", eMain)
  val uc0 = useCases(0)
  val uc1 = useCases(1)
  val uc2 = useCases(2)

  val structuredMap = strategy.findStructuredMap(project)
  val mergedMap = strategy.findMergedStructuredMap(structuredMap)
  val report = strategy.mergedMapToReportable(mergedMap, "")
  def get(key: String, children: Reportable*): Reportable = mergedMap(key) match {
    case m: MergedReportable => m.copy(children = children.toList)
    case s: SimpleRequirementAndHolder => s.copy(children = children.toList)
  }
  assertEquals(Some("UC 1"), uc0.title)

  "A ByReferenceDocumentPrinterStrategy" should "provide a nice number for each reportable" in {
    val reportableToPath = strategy.findReportableToPathFor(project)
    val reportableToRef = strategy.findReportableToRef(project, reportableToPath)
    assertEquals("", reportableToRef(project))
    assertEquals("1", reportableToRef(eMain))
    assertEquals("1.1", reportableToRef(useCases(0)))
    assertEquals("1.1.1", reportableToRef(scenarios(0)))
    assertEquals("7.3", reportableToRef(scenarios(1)))
    assertEquals("1.1.3", reportableToRef(scenarios(2)))

    assertEquals("1.2", reportableToRef(useCases(1)))
    assertEquals("1.2.1", reportableToRef(scenarios(3)))
    assertEquals("1.2.2", reportableToRef(scenarios(4)))

    assertEquals("1.3", reportableToRef(useCases(2)))
    assertEquals("1.3.1", reportableToRef(scenarios(5)))
    assertEquals("1.3.2", reportableToRef(scenarios(6)))
  }

  it should "add each reportable to the structured map" in {
    def check(key: String, re: RequirementAndEngine*) {
      assertEquals(re.toList, structuredMap(key))
    }
    check("1", (eMain, eMain))
    check("1.1", (uc0, eMain))
    //    check("1.1.1", (scenarios(0), eMain))
//    check("7.3", (scenarios(1), eMain))
    //    check("1.1.3", (scenarios(2), eMain))
    check("1.2", (uc1, eMain))
    //    check("1.2.1", (scenarios(3), eMain))
    //    check("1.2.2", (scenarios(4), eMain))
    check("1.3", (uc2, eMain))
    //    check("1.3.1", (scenarios(5), eMain))
    //    check("1.3.2", (scenarios(6), eMain))
    //    check("", project)
  }

  it should "'merge' the values " in {
    def check(key: String, re: RequirementAndEngine*) {
      val expected = MergedReportable.makeFrom(key, re.toList, List())
      val actual = mergedMap(key)
      assertEquals(expected, actual)
    }
    check("1", (eMain, eMain))
    check("1.1", (uc0, eMain))
    //    check("1.1.1", (scenarios(0), eMain))
    //    check("7.3", (scenarios(1), eMain))
    //    check("1.1.3", (scenarios(2), eMain))
    check("1.2", (uc1, eMain))
    //    check("1.2.1", (scenarios(3), eMain))
    //    check("1.2.2", (scenarios(4), eMain))
    check("1.3", (uc2, eMain))
    //    check("1.3.1", (scenarios(5), eMain))
    //    check("1.3.2", (scenarios(6), eMain))
  }

  it should "turn the merged reportables into a reportable structure" in {
    assertEquals(SimpleRequirementAndHolder.withJustChildren(
      get("1",
        get("1.1", get("1.1.1"), get("1.1.3")),
        get("1.2", get("1.2.1"), get("1.2.2")),
        get("1.3", get("1.3.1"), get("1.3.2"))),
      get("7", get("7.3"))), report)

  }

  it should "turn an engine into a projection indexed by number" in {
    assertEquals(SimpleRequirementAndHolder.withJustChildren(
      get("1",
        get("1.1", get("1.1.1"), get("1.1.3")),
        get("1.2", get("1.2.1"), get("1.2.2")),
        get("1.3", get("1.3.1"), get("1.3.2"))),
      get("7", get("7.3"))), strategy.makeReportOfJustDocuments(project))
  }

}

