package org.cddcore.engine.test

import org.cddcore.engine._
import scala.language.implicitConversions
import org.cddcore.engine.AbstractEngine1Test
import org.cddcore.engine.tests._
import org.junit.runner._
import org.junit.runner.RunWith
import org.junit.runner.notification._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.ChildEngine

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
  import org.cddcore.engine.Engine._
  implicit def toBuildEngine[R](e: Engine) = e.asInstanceOf[BuildEngine]

  class CddRunnerForTests( engine: Engine) extends CddRunner {
    def title = "Test"
    def clazz = getClass
    def enginesToNameMap = Map(engine -> "engineName")
  }

  "An engine" should "notify started and finished for root, engine, usecase and scenario when one scenario" in {
    val engine1 = builder.useCase("uc1").
      scenario("one", "d1").
      expected("exp").build
    assertEquals(Map(), engine1.scenarioExceptionMap.map)
    assertEquals(List(
      "testStarted: Test",
      "testStarted: engineName",
      "testStarted: uc1",
      "testStarted: d1 => exp",
      "testFinished: d1 => exp",
      "testFinished: uc1",
      "testFinished: engineName",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }

  it should "notify started and finished for root, engine, usecase and scenario wihen two scenarios in same use case" in {
    val engine1 = builder.useCase("uc1").
      scenario("one", "d1").expected("exp").
      scenario("two").expected("exp2").because((p: String) => p == "two").
      build
    assertEquals(Map(), engine1.scenarioExceptionMap.map)
    assertEquals(List(
      "testStarted: Test",
      "testStarted: engineName",
      "testStarted: uc1",
      "testStarted: d1 => exp",
      "testFinished: d1 => exp",
      "testStarted: two => exp2",
      "testFinished: two => exp2",
      "testFinished: uc1",
      "testFinished: engineName",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }

  it should "report an exception while building to junit" in {
    val engine1 = test(builder.useCase("uc1").
      scenario("one", "d1").
      expected("exp").
      because((p: String) => p == "two").
      build)
    assertEquals(1, engine1.scenarioExceptionMap.map.size)
    val exception: ScenarioBecauseException = engine1.scenarioExceptionMap.map.values.head.asInstanceOf[ScenarioBecauseException]
    assertEquals(List(
      "testStarted: Test",
      "testStarted: engineName",
      "testStarted: uc1",
      "testStarted: d1 => exp",
      "testFailure: d1 => exp: ((p: String) => p.==(\"two\")) is not true for Scenario(d1, one, because=((p: String) => p.==(\"two\")), expected=exp)\nDetailed:\n  List(one)\n",
      "testFinished: uc1",
      "testFinished: engineName",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }

  it should "continue to execute after a failure" in {
    val engine1 = test(builder.useCase("uc1").
      scenario("one", "d1").
      expected("exp").
      because((p: String) => p == "two").
      scenario("three", "d3").
      expected("three").
      because((p: String) => p == "three").
      build)
    assertEquals(1, engine1.scenarioExceptionMap.map.size)
    val exception: ScenarioBecauseException = engine1.scenarioExceptionMap.map.values.head.asInstanceOf[ScenarioBecauseException]
    val actual =  runAndGetListOfNotifications(engine1)
    assertEquals(List(
      "testStarted: Test",
      "testStarted: engineName",
      "testStarted: uc1",
      "testStarted: d1 => exp",
      "testFailure: d1 => exp: ((p: String) => p.==(\"two\")) is not true for Scenario(d1, one, because=((p: String) => p.==(\"two\")), expected=exp)\nDetailed:\n  List(one)\n",
      "testStarted: d3 => three",
      "testFinished: d3 => three",
      "testFinished: uc1",
      "testFinished: engineName",
      "testFinished: Test"),actual)
  }
  
  it should "pass if the expected exception is thrown" in {
    val e = builder.
      scenario("one", "d1").expectException(new RuntimeException()).code((x: String) =>
        throw new RuntimeException).
      build
    assertEquals(List(
      "testStarted: Test",
      "testStarted: engineName",
      "testStarted: d1 => throws RuntimeException",
      "testFinished: d1 => throws RuntimeException",
      "testFinished: engineName",
      "testFinished: Test"), runAndGetListOfNotifications(e))
  }
  "An engine with child engines" should "report an exception while building to junit" in {
    val engine1 = test(new Builder1(ScenarioBuilderData(logger, arity = 1, folder = (Some(_ + _)), initialFoldValue = () => "")).
      childEngine("ce1").
      scenario("one", "d1").
      expected("exp").
      because((p: String) => p == "two").
      build)
    val map = engine1.asInstanceOf[EngineWithScenarioExceptionMap].scenarioExceptionMap.map
    assertEquals(1, map.size)
    val exception: ScenarioBecauseException = map.values.head.asInstanceOf[ScenarioBecauseException]
    assertEquals(List(
      "testStarted: Test",
      "testStarted: engineName",
      "testStarted: ce1",
      "testStarted: d1 => exp",
      "testFailure: d1 => exp: ((p: String) => p.==(\"two\")) is not true for Scenario(d1, one, because=((p: String) => p.==(\"two\")), expected=exp)\nDetailed:\n  List(one)\n",
      "testFinished: ce1",
      "testFinished: engineName",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }

  
  //This test exists because an earlier implementation of CddRunner used description as a key in a critical data structure. Two things with the "equals" description cause an unpleasant bug 
  it should "report exceptions even if the scenario name is identical" in {
    val engine1 = test(new Builder1(ScenarioBuilderData(logger, arity = 1, folder = (Some(_ + _)), initialFoldValue = () => "")).
      childEngine("ce1").
      scenario("one", "sname").
      expected("exp").
      because((p: String) => p == "two").
      scenario("one", "sname").
      expected("exp").
      because((p: String) => p == "one").
      build)
    val map = engine1.asInstanceOf[EngineWithScenarioExceptionMap].scenarioExceptionMap.map
    assertEquals(1, map.size)
    val exception: ScenarioBecauseException = map.values.head.asInstanceOf[ScenarioBecauseException]
    assertEquals(List(
      "testStarted: Test",
      "testStarted: engineName",
      "testStarted: ce1",
      "testStarted: sname => exp",
      "testFailure: sname => exp: ((p: String) => p.==(\"two\")) is not true for Scenario(sname, one, because=((p: String) => p.==(\"two\")), expected=exp)\nDetailed:\n  List(one)\n",
      "testStarted: sname => exp",
      "testFinished: sname => exp",
      "testFinished: ce1",
      "testFinished: engineName",
      "testFinished: Test"), runAndGetListOfNotifications(engine1))
  }



  
  
  def runAndGetListOfNotifications(engine: Engine) = {
    val runner = new CddRunnerForTests(engine)
    val listener = new RunListenerForTests
    val notifier = new RunNotifier()
    notifier.addFirstListener(listener)
    runner.run(notifier)
    listener.list.reverse
  }

}