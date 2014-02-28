package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

case class DisplayTest(val x: String) extends LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor) = "{" + x + "}"
}

@RunWith(classOf[JUnitRunner])
class EngineLoggerRunnerDisplayTests extends AbstractEngine1Test[DisplayTest, DisplayTest] {

  override val logger = new TestLogger()
  implicit def string_to_because(s: String) = new CodeHolder[B]((x) => x.x contains s, s.toString())
  implicit def string_to_rfn2(s: String): (DisplayTest) => DisplayTest = (x: DisplayTest) => DisplayTest(s)
  implicit def string_to_display_test(s: String) = DisplayTest(s)

  val builder_W = builder.useCase("no use case").scenario(DisplayTest("W")).expected("Z")
  val builder_W_A = builder_W.scenario(DisplayTest("A")).expected("X").because("A")
  val builder_W_A_B = builder_W_A.scenario(DisplayTest("B")).expected("Y").because("B")
  val builder_W_A_B_AB = builder_W_A_B.scenario(DisplayTest("AB")).expected("Z").because("AB")
  val e_w_a_b_ab = builder_W_A_B_AB.build

  val wS = e_w_a_b_ab.tests(0);
  val aS = e_w_a_b_ab.tests(1);
  val abS = e_w_a_b_ab.tests(2);

  "A logger display processor" should "use the display method if available" in {
    val processor = LoggerDisplayProcessor()
    assert("msg" == processor("msg"))
    assert("1" == processor(1))
    assert("{1}" == processor(DisplayTest("1")))
    assert("msg" == processor(new LoggerDisplay() { def loggerDisplay(dp: LoggerDisplayProcessor) = "msg" }))
  }

  it should "use the class function list in preference to the display method " in {
    val processor = new SimpleLoggerDisplayProcessor(ClassFunctionList(List(ClassFunction(classOf[DisplayTest], (d: DisplayTest) => "<" + d.x + ">"))))
    assert("msg" == processor("msg"))
    assert("1" == processor(1))
    assert("<1>" == processor(DisplayTest("1")))
    assert("<1>" == processor(new DisplayTest("1") {}))
  }

  "An empty engine" should "log the default solution" in {
    val e = builder_W.build
    logger.reset
    e("a")
    e("b")
    checkMessages(
      "DEBUG Run()  Executing {a}",
      "DEBUG Run()   Result {Z}",
      "DEBUG Run()  Executing {b}",
      "DEBUG Run()   Result {Z}")
  }

  it should "log executions with simple if then" in {
    val e = builder_W_A.build
    logger.reset
    e("A")
    e("B")
    checkLastMessages(
      "DEBUG Run()  Executing {A}",
      "INFO  Run()   Condition Left(CodeHolder(A)) was true",
      "DEBUG Run()   Result {X}",
      "DEBUG Run()  Executing {B}",
      "INFO  Run()   Condition Left(CodeHolder(A)) was false",
      "DEBUG Run()   Result {Z}")
  }

}
@RunWith(classOf[JUnitRunner])
class EngineLoggerRunnerStringTests extends EngineStringStringTests {

  "A logger display processor" should "use the display method if available" in {
    val processor = new SimpleLoggerDisplayProcessor() {}
    assert("msg" == processor("msg"))
    assert("1" == processor(1))
    assert("msg" == processor(new LoggerDisplay() { def loggerDisplay(dp: LoggerDisplayProcessor) = "msg" }))
  }

  //  "An empty engine" should "log the default solution" in {
  //    val engine: Engine1[String, String] = Engine1[String, String]("Z": RFn, emptyUsecase).withLogger(new TestLogger());
  //    engine("a")
  //    engine("b")
  //    checkMessages(engine,
  //      "DEBUG Run()  Executing a",
  //      "DEBUG Run()   Result Z",
  //      "DEBUG Run()  Executing b",
  //      "DEBUG Run()   Result Z")
  //  }
  //
  //  it should "log executions with simple if then" in {
  //    val emptyUsecase = UseCase[B, RFn, String]("")
  //    val scenario = Scenario[String, String]("A").becauseBecause("A").produces("X")
  //    val engine = Engine1[String, String]("Z": RFn, UseCase("", scenario)).withLogger(new TestLogger());
  //    assert("X" == engine("A"))
  //    assert("Z" == engine("B"))
  //    checkLastMessages(engine,
  //      "DEBUG Run()  Executing A",
  //      "INFO  Run()   ConditionBecause(A) was true",
  //      "DEBUG Run()   Result X",
  //      "DEBUG Run()  Executing B",
  //      "INFO  Run()   ConditionBecause(A) was false",
  //      "DEBUG Run()   Result Z")
  //  }

}