package org.cddcore.engine

import scala.language.implicitConversions
import junit.framework.AssertionFailedError
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.scalatest.FlatSpecLike
import scala.xml.Node

trait AssertEquals {
  def assertEquals[T1, T2](expected: T1, actual: T2) {
    def msg = "\nExpected\n" + expected + "\nActual\n" + actual
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
          ShouldMatchers.fail("First fail at " + i + " Expected: [" + expectedString.substring(i, expectedMax) + "] Actual: [ " + actualString.substring(i, actualMax) + "]\n" + msg)
        }
      }
      expectedString.length() - actualString.length() match {
        case x if x < 0 => ShouldMatchers.fail(s"Actual ran over end at ${expectedString.length}\n" + msg)
        case x if x > 0 => ShouldMatchers.fail(s"Actual fell short end at ${actualString.length}\n" + msg)
      }

    }
    assert(expected == actual, msg)
  }

  def assertTextEquals(expected: String, actual: Node) {
    assertEquals(expected, actual.text.trim)
  }

  def assertEquals[T1, T2](prefix: String, expected: T1, actual: T2) {
    assert(expected == actual, prefix + "\nExpected\n" + expected + "\nActual\n" + actual)
  }
}

trait AbstractTest extends FlatSpecLike with ShouldMatchers with AssertEquals {

}

trait AbstractEngineTest[R] extends AbstractTest with EngineUniverse[R] with NodeComparator[R] {
  def logger: TddLogger
  def builder: RealScenarioBuilder
  def firstUseCaseDescription = "UseCase1"
  def builderWithUseCase: RealScenarioBuilder = builder.useCase(firstUseCaseDescription)
  def checkScenariosExist[X](engine: Engine, expected: String*) {
    assert(engine.scenarios.size == expected.size)
    for ((c, a) <- (engine.scenarios, expected).zipped) {
      assert(c.becauseString == a, "Expected: [" + a + "]\nBecauseString = [" + c.becauseString + "]\n\nActual " + c + "\n   Scenarios: " + engine.scenarios + "\nEngine:\n" + engine)
    }
  }
  def assertEngineMatches(e: Engine, n2: RorN) {
    val actual = compareNodes(e.root.asInstanceOf[RorN], n2)
    assert(actual == List(), "\n" + actual.mkString("\n\n") + "\n\nExpected:\n" + n2 + "\n\nRoot:\n" + e.root)
  }

  //  def checkSingleException[E](code: => Unit, exceptionClass: Class[E]): E =
  //    try {
  //      code
  //      throw new AssertionFailedError("Expected " + exceptionClass.getName());
  //    } catch {
  //      case e: Exception if exceptionClass == e.get  => 
  //        assert(1==e.scenarioExceptionMap.size, s"Expected one exception of type $exceptionClass got ${e.scenarioExceptionMap}")
  //        e.scenarioExceptionMap.keys.head.asInstanceOf[E]
  //    }
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

trait AbstractEngine1Test[P, R] extends BuilderFactory1[P, R] with AbstractEngineTest[R] with Engine1Types[P, R]
trait AbstractEngine2Test[P1, P2, R] extends BuilderFactory2[P1, P2, R] with AbstractEngineTest[R] with Engine2Types[P1, P2, R]
trait AbstractEngine3Test[P1, P2, P3, R] extends BuilderFactory3[P1, P2, P3, R] with AbstractEngineTest[R] with Engine3Types[P1, P2, P3, R]

trait FirstScenarioTest[R] extends AbstractEngineTest[R] {
  def builderWithScenario: RealScenarioBuilder;
  def firstScenario: Scenario = scenarioLens.get(builderWithScenario)
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
  implicit def string_to_result(s: String) = new CodeFn[B, RFn, String]((p: P) => s, s.toString())

}

trait DefaultBecauseForStrings1[R] extends AbstractEngine1Test[String, R] {
  val becauseA = new Because[B]((h => h contains "A"), "hA");
  val becauseB = new Because[B]((h => h contains "B"), "hB");
  val becauseAB = new Because[B]((h => (h contains "A") & (h contains "B")), "hAB");
}
trait DefaultScenarioAndBecauseForStrings1 extends AbstractEngine1Test[String, String] {
  lazy val builderWithScenario = builderWithUseCase.scenario("W")
  lazy val builderWithDefault = builderWithScenario.expected("Z");
  lazy val engineWithDefault = builderWithDefault.build
  lazy val defaultScenario = engineWithDefault.scenarios.head
}

trait EngineStringStringTests extends AbstractEngine1Test[String, String] with EngineString1Tests[String] with DefaultScenarioAndBecauseForStrings1 with DefaultBecauseForStrings1[String] {
  implicit def string_to_because(s: String) = new Because[B]((x) => x contains s, s.toString())

}