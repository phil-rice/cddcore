package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

trait DocumentPrinterTestFramework {
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
  val eSecond = Engine[Int, String]().title("Main Engine").
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

}

@RunWith(classOf[JUnitRunner])
class DefaultMergeStrategyTest extends AbstractTest with DocumentPrinterTestFramework with ReportableTestFramework {
  import Key._
  val documentMergeStrategy = DocumentMergeStrategy.default

  "The default merge strategy toRequirementAndEngineMethod" should "turn only requirement and holders to RequirementAndEngines as we don't want to see the scenarios as their own title" in {
    assertEquals(Some((eMain, eMain): RequirementAndEngine), documentMergeStrategy.toRequirementAndEngine(eMain, Some(eMain)))
    assertEquals(Some((uc0, eMain): RequirementAndEngine), documentMergeStrategy.toRequirementAndEngine(uc0, Some(eMain)))
    assertEquals(None, documentMergeStrategy.toRequirementAndEngine(scenarios(0), Some(eMain)))
  }

  "The default merge strategy merge method" should "turn the empty list into a placeholder" in {
    assertEquals(SimpleRequirementAndHolder(), documentMergeStrategy.merge("any", List()))
  }

  it should "turn a single use case into a reportable / title / description / wrapper, leaving the scenarios under the wrapper" in {
    val uc0list = List(uc0: RequirementAndEngine)
    val expected = MergedReportable("some key",
      List(MergedTitle(uc0.title,
        List(MergedDescription(uc0.description,
          List(SimpleRequirementAndHolder(uc0: RequirementAndEngine, uc0.children): RequirementAndEngine))))),
      List())
    val actual = documentMergeStrategy.merge("some key", uc0list)
    assertEquals(expected, actual)
  }

  it should "turn an engine into a reportable / title / description / wrapper, leaving only  scenarios under the wrapper (loosing the use cases)" in {
    val engineList = List(eSecond: RequirementAndEngine)
    val scenarios = eSecond.asInstanceOf[EngineBuiltFromTests[_]].tests
    val expected = MergedReportable("some key",
      List(MergedTitle(eSecond.title,
        List(MergedDescription(eSecond.description,
          List(SimpleRequirementAndHolder(eSecond: RequirementAndEngine, List(scenarios(2), scenarios(1), scenarios(0))): RequirementAndEngine))))),
      List())
    val actual = documentMergeStrategy.merge("some key", engineList)
    assertEquals(expected, actual)

  }

}

@RunWith(classOf[JUnitRunner])
class ChangedMergeStrategyTest extends AbstractTest with DocumentPrinterTestFramework {
  import Key._

}

@RunWith(classOf[JUnitRunner])
class ByReferenceDocumentPrinterStrategyTest extends AbstractTest with DocumentPrinterTestFramework {
  import Key._

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
    check("1.2", (uc1, eMain))
    check("1.3", (uc2, eMain))
  }

  it should "'merge' the values " in {
    def check(key: String, re: RequirementAndEngine*) {
      val expected = DocumentMergeStrategy.default.makeFrom(key, re.toList, List())
      val actual = mergedMap(key)
      assertEquals(expected, actual)
    }
    check("1", (eMain, eMain))
    check("1.1", (uc0, eMain))
    check("1.2", (uc1, eMain))
    check("1.3", (uc2, eMain))
  }

  it should "turn the merged reportables into a reportable structure" in {
    assertEquals(SimpleRequirementAndHolder.withJustChildren(
      get("1",
        get("1.1"),
        get("1.2"),
        get("1.3"))), report)
  }

  it should "turn an engine into a projection indexed by number" in {
    assertEquals(SimpleRequirementAndHolder.withJustChildren(
      get("1",
        get("1.1"),
        get("1.2"),
        get("1.3"))), strategy.makeReportOfJustDocuments(project))
  }

}

