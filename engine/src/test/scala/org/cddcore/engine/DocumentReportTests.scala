package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

trait DocumentReportTestFixture {
  val eMain = Engine[Int, String]().title("Main Engine").
    useCase("UC 1", "The body of the first use case").reference("r1", None).priority(3).
    scenario(11).expected("one-one").because((x: Int) => x == 11).
    scenario(12).expected("one-two").because((x: Int) => x == 12).

    useCase("UC 2", "The body of the second use case").
    scenario(21).expected("two-one").because((x: Int) => x == 21).
    scenario(22).expected("two-two").because((x: Int) => x == 22).

    useCase("UC 3", "The body of the third use case").
    scenario(31).expected("three-one").because((x: Int) => x == 31).
    scenario(32).expected("three-two").because((x: Int) => x == 32).
    build

}

@RunWith(classOf[JUnitRunner])
class DocumentReportTests extends AbstractTest with DocumentReportTestFixture {

  val usecases = eMain.all(classOf[UseCase])
  val scenarios = eMain.all(classOf[Test])

  def uc(i: Int) = SimpleRequirementAndHolder(usecases(i))

  "SimpleRequirementAndHolder apply method" should "make a projection of a usecase passed " in {
    val uc0 = usecases(0)
    val modified = SimpleRequirementAndHolder(uc0).asInstanceOf[SimpleRequirementAndHolder]
    assertEquals(uc0.title, modified.title)
    assertEquals(uc0.description, modified.description)
    assertEquals(uc0.priority, modified.priority)
    assertEquals(uc0.references, modified.references)
    assertEquals(uc0.children, modified.children)
    assertEquals(uc0, modified.delegate.get)
    assertEquals(scenarios(0), SimpleRequirementAndHolder(scenarios(0)))
  }

  it should "make a projection of an engine passed in" in {
    val modified = SimpleRequirementAndHolder(eMain).asInstanceOf[SimpleRequirementAndHolder]
    assertEquals(eMain.title, modified.title)
    assertEquals(eMain.description, modified.description)
    assertEquals(eMain.priority, modified.priority)
    assertEquals(eMain.references, modified.references)
    assertEquals(List(uc(2), uc(1), uc(0)), modified.children)
    assertEquals(eMain, modified.delegate.get)
  }

  it should "return parameter if passed a SimpleRequirementAndHolder" in {
    val modified = SimpleRequirementAndHolder(eMain).asInstanceOf[SimpleRequirementAndHolder]
    assertEquals(modified, SimpleRequirementAndHolder(modified))

  }
  "A SimpleDocumentPrinterStrategy when given an engine with simple use cases and scenarios" should "return the simple projection" in {
    val originalReport = Report("Report Title", eMain)
    val report = new SimpleDocumentPrinterStrategy().makeReportOfJustDocuments(originalReport).asInstanceOf[RequirementAndHolder]
    assertEquals(List(SimpleRequirementAndHolder(eMain)), report.children)
    assertEquals(originalReport.title, report.title)
  }
}

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
    val reportableToRef = strategy.findReportableToRef(report)
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
    val structuredMap = strategy.findStructuredMap(report)
    assertEquals(List(report), structuredMap(""))
    assertEquals(List(eMain), structuredMap("1"))
    assertEquals(List(uc0), structuredMap("1.1"))
    assertEquals(List(scenarios(0)), structuredMap("1.1.1"))
    assertEquals(List(scenarios(1)), structuredMap("7.3"))
    assertEquals(List(scenarios(2)), structuredMap("1.1.3"))
    assertEquals(List(uc1), structuredMap("1.2"))
    assertEquals(List(scenarios(3)), structuredMap("1.2.1"))
    assertEquals(List(scenarios(4)), structuredMap("1.2.2"))
    assertEquals(List(uc2), structuredMap("1.3"))
    assertEquals(List(scenarios(5)), structuredMap("1.3.1"))
    assertEquals(List(scenarios(6)), structuredMap("1.3.2"))
  }
  
}

