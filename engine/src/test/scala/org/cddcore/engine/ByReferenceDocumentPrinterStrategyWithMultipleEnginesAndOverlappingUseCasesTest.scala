package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class ByReferenceDocumentPrinterStrategyWithMultipleEnginesAndOverlappingUseCasesTest extends AbstractTest {
  val document = new Document(Some("Private Pensions Document"))
  val mainCarers = Engine[Int, String]().title("Main Carers").
    useCase("Private Pensions", "Private Pensions Description1").reference("2.1", document).
    scenario(11).expected("one-one").because((x: Int) => x == 11).
    scenario(12).expected("one-two").because((x: Int) => x == 12).reference("7.3", None).
    scenario(13).expected("one-three").because((x: Int) => x == 13).
    useCase("Should be", "ignored").
    scenario(61).expected("six-one").because((x: Int) => x == 61).
    scenario(62).expected("six-two").because((x: Int) => x == 62).
    build

  val nettIncome = Engine[Int, String]().title("Nett Income").
    useCase("Private Pensions", "Private Pensions Description2").reference("2.1", document).
    scenario(21).expected("two-one").because((x: Int) => x == 21).
    scenario(22).expected("two-two").because((x: Int) => x == 22).
    build

  val income = Engine[Int, String]().title(" Income").
    useCase("Private Pensions", "Private Pensions Description1").reference("2.1", document).
    scenario(31).expected("three-one").because((x: Int) => x == 31).
    scenario(32).expected("three-two").because((x: Int) => x == 32).
    build

  val privatePension = Engine[Int, String]().title("Private Pension").reference("2", document).
    useCase("Private Pensions", "Private Pensions Description1").
    scenario(41).expected("four-one").because((x: Int) => x == 41).
    scenario(42).expected("four-two").because((x: Int) => x == 42).
    useCase("Private Pensions 2", "More Private Pensions Description").
    scenario(51).expected("five-one").because((x: Int) => x == 51).
    scenario(52).expected("five-two").because((x: Int) => x == 52).
    build

  val otherEngine = Engine[Int, String]().title("Nothing to do with pensions").
    scenario(72).expected("seven-two").because((x: Int) => x == 72).
    build

  val project = Project("Carers", mainCarers, nettIncome, income, otherEngine, privatePension)
  val rawReport = Report("Document View", project);
  val strategy = new ByReferenceDocumentPrinterStrategy(Some(document), new SimpleKeyStrategy(), debug = true)
  val report = strategy.makeReportOfJustDocuments(rawReport).asInstanceOf[SimpleRequirementAndHolder]

  def findUseCase(e: Engine, i: Int) = e.all(classOf[UseCase])(i)
  val mainCarersUseCase = findUseCase(mainCarers, 0)
  val nettIncomeUseCase = findUseCase(nettIncome, 0)
  val incomeUseCase = findUseCase(income, 0)
  val privatePensionUseCase = findUseCase(privatePension, 0)


 "A ByReferenceDocumentPrinterStrategy"  should "build up a structured map that just has the reportables referenced" in {
    import scala.language.implicitConversions
    val structuredMap = strategy.findStructuredMap(rawReport)
    implicit def reqToRequirementAndNone(r: RequirementAndHolder) = new RequirementAndEngine(r, None)
    implicit def tupleToRequirementAndSomeEngine(t: (RequirementAndHolder, Engine)) = new RequirementAndEngine(t._1, Some(t._2))
    def check(key: String, re: RequirementAndEngine*) {
      assertEquals(re.toList, structuredMap(key))
    }

    check("2", (privatePension, privatePension))
    check("2.1", (mainCarersUseCase, mainCarers), (nettIncomeUseCase, nettIncome), (incomeUseCase, income), (privatePensionUseCase, privatePension))

  }
}