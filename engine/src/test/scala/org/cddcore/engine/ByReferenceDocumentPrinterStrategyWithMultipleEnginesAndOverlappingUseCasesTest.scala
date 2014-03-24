package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
trait SomeEngines {
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

  case class Person(hasPet: Boolean = false, hasWife: Boolean = false, isRich: Boolean = false, isInTrouble: Boolean = false, hasWorkIssues: Boolean = false)

  val happy = Engine.folding[Person, Boolean, Boolean]((acc: Boolean, h: Boolean) => acc && h, true).
    title("Happy Engine").
    childEngine("Happy people have pets").priority(1).
    scenario(Person(hasPet = true), "s1").expected(true).because((p: Person) => p.hasPet).priority(1).
    scenario(Person(hasPet = false), "s2").expected(false).because((p: Person) => !p.hasPet).priority(0).
    childEngine("Happy people have wives").priority(1).
    scenario(Person(hasWife = false), "s3").expected(false).because((p: Person) => !p.hasWife).priority(0).
    scenario(Person(hasWife = true), "s4").expected(true).because((p: Person) => p.hasWife).priority(1).
    childEngine("Happy people are rich").
    scenario(Person(isRich = false)).expected(false).
    scenario(Person(isRich = true)).expected(true).because((p: Person) => p.isRich).
    childEngine("Happy people are not in trouble").
    scenario(Person(isInTrouble = true)).expected(false).
    scenario(Person(isInTrouble = false)).expected(true).because((p: Person) => !p.isInTrouble).
    childEngine("Happy people don't have work issues").
    scenario(Person(hasWorkIssues = true)).expected(false).
    scenario(Person(hasWorkIssues = false)).expected(true).because((p: Person) => !p.hasWorkIssues).
    build;

  val project = Project("Carers", mainCarers, nettIncome, income, otherEngine, privatePension)
  def findUseCase(e: Engine, i: Int) = e.all(classOf[UseCase])(i)
  def reqWith(e: Engine, title: String) = e.all(classOf[Requirement]).find(_.titleString == title).get
  def orderOf(e: Engine, title: String) = reqWith(e, title).asInstanceOf[ReportableWithTextOrder].textOrder
  def scenarioWith(e: Engine, i: Int) = e.all(classOf[Test]).find(_.params(0) == i).get
}

@RunWith(classOf[JUnitRunner])
class TextOrderTests extends AbstractTest with SomeEngines {

  "An engine" should "have the test order field set in it" in {
    val e1 = orderOf(happy, "Happy people have pets")
    val e2 = orderOf(happy, "Happy people have wives")
    val e3 = orderOf(happy, "Happy people are rich")
    val e4 = orderOf(happy, "Happy people are not in trouble")
    val e5 = orderOf(happy, "Happy people don't have work issues")
    assert(e1 < e2)
    assert(e2 < e3)
    assert(e3 < e4)
    assert(e4 < e5)
  }

  "A usecase" should "have the test order field set in it" in {
    val u1 = orderOf(happy, "Happy people have pets")
    val u2 = orderOf(happy, "Happy people have wives")
    val u3 = orderOf(happy, "Happy people are rich")
    val u4 = orderOf(happy, "Happy people are not in trouble")
    val u5 = orderOf(happy, "Happy people don't have work issues")
    assert(u1 < u2)
    assert(u2 < u3)
    assert(u3 < u4)
    assert(u4 < u5)
  }
  "A scenario" should "have the test order field set in it" in {
    val s1 = orderOf(happy, "s1")
    val s2 = orderOf(happy, "s2")
    val s3 = orderOf(happy, "s3")
    val s4 = orderOf(happy, "s4")
    assert(s1 < s2)
    assert(s2 < s3)
    assert(s3 < s4)
  }
}

@RunWith(classOf[JUnitRunner])
class ByReferenceDocumentPrinterStrategyWithMultipleEnginesAndOverlappingUseCasesTest extends AbstractTest with SomeEngines {

  val rawReport = Report("Document View", project);
  val strategy = new ByReferenceDocumentPrinterStrategy(Some(document), new SimpleKeyStrategy(), debug = false)
  val report = strategy.makeReportOfJustDocuments(rawReport).asInstanceOf[SimpleRequirementAndHolder]

  val mainCarersUseCase = findUseCase(mainCarers, 0)
  val nettIncomeUseCase = findUseCase(nettIncome, 0)
  val incomeUseCase = findUseCase(income, 0)
  val privatePensionUseCase = findUseCase(privatePension, 0)

  "A ByReferenceDocumentPrinterStrategy" should "build up a structured map that just has the reportables referenced" in {
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