package org.cddcore.engine
import org.scalatest.FlatSpecLike
import scala.xml.Node
import org.scalatest.Matchers
import scala.runtime.ZippedTraversable2.zippedTraversable2ToTraversable
import org.scalatest.Finders
import scala.language.implicitConversions

trait AssertEquals {
  def assertEquals[T1, T2](expected: T1, actual: T2, prefix: String = "") {
    def msg = prefix +"\nExpected\n" + expected + "\nActual\n" + actual
    if (expected.isInstanceOf[String] & actual.isInstanceOf[String]) {
      val expectedString = expected.asInstanceOf[String];
      val actualString = actual.asInstanceOf[String];
      if (expectedString == actualString)
        return ;
      val s: Traversable[Tuple2[Char, Char]] = for ((ce, ca) <- (expectedString, actualString).zipped) yield (ce, ca)
      for ((t, i) <- s.toList.zipWithIndex) {
        if (t._1 != t._2) {
          val expectedMax = Math.min(i + 10, expectedString.length() - 1)
          val actualMax = Math.min(i + 10, actualString.length() - 1)
          Matchers.fail("First fail at " + i + " Expected: [" + expectedString.substring(i, expectedMax) + "] Actual: [ " + actualString.substring(i, actualMax) + "]\n" + msg)
        }
      }
      expectedString.length() - actualString.length() match {
        case x if x < 0 => Matchers.fail(s"Actual ran over end at ${expectedString.length}\n" + msg)
        case x if x > 0 => Matchers.fail(s"Actual fell short end at ${actualString.length}\n" + msg)
      }

    }
    if (expected != actual)
      assert(expected == actual, msg)
  }

  def assertTextEquals(expected: String, actual: Node) {
    assertEquals(expected, actual.text.trim)
  }

  def assertEquals[T1, T2](prefix: String, expected: T1, actual: T2) {
    assert(expected == actual, prefix + "\nExpected\n" + expected + "\nActual\n" + actual)
  }
}

trait AbstractTest extends FlatSpecLike with Matchers with AssertEquals {

}

trait AbstractEngineTest[R] extends AbstractTest with EngineUniverse[R, R] with NodeComparator[R, R] {
  implicit def test_to_scenario(t: Test) = t.asInstanceOf[Scenario]
  implicit def toEngineFromTests[R](e: Engine) = e.asInstanceOf[EngineBuiltFromTests[String]]

  def logger: TddLogger
  def builder: RealScenarioBuilder
  def firstUseCaseDescription = "UseCase1"
  def builderWithUseCase: RealScenarioBuilder = builder.useCase(firstUseCaseDescription)
  def checkScenariosExist[X](engine: EngineFromTestsImpl, expected: String*) {
    assert(engine.tests.size == expected.size)
    for ((c, a) <- (engine.tests, expected).zipped) {
      assert(c.becauseString == a, "Expected: [" + a + "]\nBecauseString = [" + c.becauseString + "]\n\nActual " + c + "\n   Scenarios: " + engine.tests + "\nEngine:\n" + engine)
    }
  }
  def allScenariosForBuild(b: ScenarioBuilder) = Reportable.allTests(b.builderData.childrenModifiedForBuild)
  def firstScenario(b: ScenarioBuilder): Scenario = b.builderData.all(classOf[Scenario])(0)
  def firstScenario(e: ReportableHolder): Scenario = e.all(classOf[Scenario])(0)

  def assertEngineMatches[P](e: Engine, n2: RorN) {
    val actual = compareNodes(e.root.asInstanceOf[RorN], n2)
    assert(actual == List(), "\n" + actual.mkString("\n\n") + "\n\nExpected:\n" + n2 + "\n\nRoot:\n" + e.root)
  }

  def checkMessages[X](expected: String*) {
    val actual = logger.asInstanceOf[TestLogger].messages
    assert(expected.toList == actual, "\nExpected: " + expected + "\nActual: " + actual)
  }
  def checkLastMessages(expected: String*) {
    val full = logger.asInstanceOf[TestLogger].messages
    val actual = full.takeRight(expected.size)
    assert(expected.toList == actual, "\nExpected: " + expected + "\nActual: " + full)
  }
}

class AbstractEngine1Test[P, R] extends BuilderFactory1[P, R, R](None, Engine.noInitialValue) with AbstractEngineTest[R] with Engine1Types[P, R, R]
class AbstractEngine2Test[P1, P2, R] extends BuilderFactory2[P1, P2, R, R](None, Engine.noInitialValue) with AbstractEngineTest[R] with Engine2Types[P1, P2, R, R]
class AbstractEngine3Test[P1, P2, P3, R] extends BuilderFactory3[P1, P2, P3, R, R](None, Engine.noInitialValue) with AbstractEngineTest[R] with Engine3Types[P1, P2, P3, R, R]

trait FirstScenarioTest[R] extends AbstractEngineTest[R] {
  def builderWithScenario: RealScenarioBuilder;
  def firstScenario: Scenario = firstScenario(builderWithScenario)
  def firstParams: List[Any]
}

trait FirstScenario1Test[P, R] extends AbstractEngine1Test[P, R] with FirstScenarioTest[R] {
  def builderWithScenario = builderWithUseCase.scenario(firstParams(0).asInstanceOf[P])
}

trait FirstScenario2Test[P1, P2, R] extends AbstractEngine2Test[P1, P2, R] with FirstScenarioTest[R] {
  def builderWithScenario = builderWithUseCase.scenario(firstParams(0).asInstanceOf[P1], firstParams(1).asInstanceOf[P2])
}

trait FirstScenario3Test[P1, P2, P3, R] extends AbstractEngine3Test[P1, P2, P3, R] with FirstScenarioTest[R] {
  def builderWithScenario = builderWithUseCase.scenario(firstParams(0).asInstanceOf[P1], firstParams(1).asInstanceOf[P2], firstParams(2).asInstanceOf[P3])
}

trait EngineStringTests extends AbstractEngineTest[String] {
  implicit def string_to_rfn(s: String) = (x: String) => s
  implicit def string_to_b(s: String) = (x: String) => x contains s
}

trait EngineString1Tests[P] extends AbstractEngine1Test[P, String] with EngineStringTests {
  implicit def string_to_result(s: String) = new CodeHolder[RFn]((p: P) => s, s.toString())

}

trait DefaultBecauseForStrings1[R] extends AbstractEngine1Test[String, R] {
  val becauseA = new CodeHolder[B]((h => h contains "A"), "hA");
  val becauseB = new CodeHolder[B]((h => h contains "B"), "hB");
  val becauseAB = new CodeHolder[B]((h => (h contains "A") & (h contains "B")), "hAB");
}
trait DefaultScenarioAndBecauseForStrings1 extends AbstractEngine1Test[String, String] {
  lazy val builderWithScenario = builderWithUseCase.scenario("W")
  lazy val builderWithDefault = builderWithScenario.expected("Z");
  lazy val engineWithDefault = builderWithDefault.build
  lazy val defaultScenario = engineWithDefault.tests.head
}

trait EngineStringStringTests extends AbstractEngine1Test[String, String] with EngineString1Tests[String] with DefaultScenarioAndBecauseForStrings1 with DefaultBecauseForStrings1[String] {
  implicit def string_to_because(s: String) = new CodeHolder[B]((x) => x contains s, s.toString())

}