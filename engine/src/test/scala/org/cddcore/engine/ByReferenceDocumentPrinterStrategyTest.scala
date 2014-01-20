package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ByReferenceDocumentPrinterStrategyTest extends AbstractTest {
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
  val strategy = new ByReferenceDocumentPrinterStrategy(None, new SimpleKeyStrategy)
  val report = Report("Some title", eMain)
  val uc0 = useCases(0)
  val uc1 = useCases(1)
  val uc2 = useCases(2)

  assertEquals(Some("UC 1"), uc0.title)

  "A ByReferenceDocumentPrinterStrategy" should "turn an engine into a projection indexed by number" in {
    val projectedUc1 = SimpleRequirementAndHolder(uc0, List(scenarios(0), scenarios(2)))
    val projectedUc2 = SimpleRequirementAndHolder(uc1, List(scenarios(3), scenarios(4)))
    val projectedUc3 = SimpleRequirementAndHolder(uc2, List(scenarios(5), scenarios(6)))
    val projectedEngine = SimpleRequirementAndHolder(eMain, List(projectedUc1, projectedUc2, projectedUc3))
    val rubbish7 = new SimpleRequirementAndHolder(None, None, None, None, Set(), List(scenarios(1)))
    val projectedReport = SimpleRequirementAndHolder(report, List(projectedEngine, rubbish7))
    val actual = strategy.makeReportOfJustDocuments(report)
    assertEquals(projectedReport, actual)
  }

  it should "provide a nice number for each reportable" in {
    val reportableToPath = strategy.findReportableToPathFor(report)
    val reportableToRef = strategy.findReportableToRef(report, reportableToPath)
    assertEquals("", reportableToRef(report))
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
    import scala.language.implicitConversions
    val structuredMap = strategy.findStructuredMap(report)
    implicit def reqToRequirementAndNone(r: Requirement) = new RequirementAndEngine(r, None)
    implicit def tupleToRequirementAndSomeEngine(t: (Requirement, Engine)) = new RequirementAndEngine(t._1, Some(t._2))
    def check(key: String, re: RequirementAndEngine*) {
      assertEquals(re.toList, structuredMap(key))
    }
    check("", report)
    check("1", (eMain, eMain))
    check("1.1", (uc0, eMain))
    check("1.1.1", (scenarios(0), eMain))
    check("7.3", (scenarios(1), eMain))
    check("1.1.3", (scenarios(2), eMain))
    check("1.2", (uc1, eMain))
    check("1.2.1", (scenarios(3), eMain))
    check("1.2.2", (scenarios(4), eMain))
    check("1.3", (uc2, eMain))
    check("1.3.1", (scenarios(5), eMain))
    check("1.3.2", (scenarios(6), eMain))
  }
}

