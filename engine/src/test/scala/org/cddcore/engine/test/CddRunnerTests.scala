package org.cddcore.engine.test

import org.cddcore.engine._
import org.cddcore.engine.AbstractEngine1Test
import org.cddcore.engine.tests._
import org.junit.runner._
import org.junit.runner.RunWith
import org.junit.runner.notification._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.junit.JUnitRunner

class RunListenerForTests extends RunListener {
  var list = List[String]()

  def log(name: String, param: Any) = list = (name + ": " + param) :: list

  override def testRunStarted(description: Description) = log("testRunStarted", description)
  override def testRunFinished(result: Result) = log("testRunFinished", result)
  override def testStarted(description: Description) = log("testStarted", description)
  override def testFinished(description: Description) = log("testFinished", description)
  override def testFailure(failure: Failure) = log("testFailure", failure)
  override def testAssumptionFailure(failure: Failure) = log("testAssumptionFailure", failure)
  override def testIgnored(description: Description) = log("testIgnored", description)
}
@RunWith(classOf[JUnitRunner])
class CddRunnerTests extends AbstractEngine1Test[String, String] {
  class CddRunnerForTests extends CddRunner {
    val getDescription = Description.createSuiteDescription("Test")
  }

  "An engine" should "notify started and finished for root, engine, usecase and scenario wihen one scenario" in {
    val engine1 = builder.useCase("uc1").
      scenario("one", "d1").
      expected("exp").build
    assertEquals(Map(), engine1.scenarioExceptionMap.map)
    assertEquals(List(
      "testStarted: Test",
      "testStarted: Engine",
      "testStarted: uc1",
      "testStarted: d1 => exp ", //the space is there to separate the because
      "testFinished: d1 => exp ",
      "testFinished: uc1",
      "testFinished: Engine",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }
  "An engine" should "notify started and finished for root, engine, usecase and scenario wihen two scenarios in same use case" in {
    val engine1 = builder.useCase("uc1").
      scenario("one", "d1").
      expected("exp").
      scenario("two").
      expected("exp2").
      because((p: String) => p == "two").
      build
    assertEquals(Map(), engine1.scenarioExceptionMap.map)
    assertEquals(List(
      "testStarted: Test",
      "testStarted: Engine",
      "testStarted: uc1",
      "testStarted: d1 => exp ", //the space is there to separate the because
      "testFinished: d1 => exp ",
      "testStarted: two => exp2 <<p String> => p.==<two>>",
      "testFinished: two => exp2 <<p String> => p.==<two>>",
      "testFinished: uc1",
      "testFinished: Engine",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }

  "An engine" should "report an exception while building to junit" in {
    val engine1 = EngineTest.test(() => builder.useCase("uc1").
      scenario("one", "d1").
      expected("exp").
      because((p: String) => p == "two").
      build)
    assertEquals(1, engine1.scenarioExceptionMap.map.size)
    val exception: ScenarioBecauseException = engine1.scenarioExceptionMap.map.values.head.asInstanceOf[ScenarioBecauseException]
    assertEquals(List(
      "testStarted: Test",
      "testStarted: Engine",
      "testStarted: uc1",
      "testStarted: d1 => exp <<p String> => p.==<two>>",
      "testFailure: d1 => exp <<p String> => p.==<two>>: ((p: String) => p.==(\"two\")) is not true for Scenario(d1, one, because=((p: String) => p.==(\"two\")), expected=exp)\nDetailed:\n  List(one)",
      "testFinished: uc1",
      "testFinished: Engine",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }

  def runAndGetListOfNotifications(engine: Engine) = {
    val runner = new CddRunnerForTests
    runner.addEngineForTest("Engine", engine)
    val listener = new RunListenerForTests
    val notifier = new RunNotifier()
    notifier.addFirstListener(listener)
    runner.run(notifier)
    listener.list.reverse
  }

}