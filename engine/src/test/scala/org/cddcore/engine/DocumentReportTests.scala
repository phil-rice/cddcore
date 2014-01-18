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
    assertEquals(uc0, modified.delegate)
    assertEquals(scenarios(0), SimpleRequirementAndHolder(scenarios(0)))
  }

  it should "make a projection of an engine passed in" in {
    val modified = SimpleRequirementAndHolder(eMain).asInstanceOf[SimpleRequirementAndHolder]
    assertEquals(eMain.title, modified.title)
    assertEquals(eMain.description, modified.description)
    assertEquals(eMain.priority, modified.priority)
    assertEquals(eMain.references, modified.references)
    assertEquals(List(uc(2), uc(1), uc(0)), modified.children)
    assertEquals(eMain, modified.delegate)
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
class DocumentReportWithReferenceTest extends AbstractTest {
  val eMain = Engine[Int, String]().title("Main Engine").
    useCase("UC 1", "The body of the first use case").reference("1", None).priority(3).
    scenario(11).expected("one-one").because((x: Int) => x == 11).
    scenario(12).expected("one-two").because((x: Int) => x == 12).reference("7.3", None).

    useCase("UC 2", "The body of the second use case").
    scenario(21).expected("two-one").because((x: Int) => x == 21).
    scenario(22).expected("two-two").because((x: Int) => x == 22).

    useCase("UC 3", "The body of the third use case").
    scenario(31).expected("three-one").because((x: Int) => x == 31).
    scenario(32).expected("three-two").because((x: Int) => x == 32).
    build
}