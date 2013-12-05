package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineDefaultTests extends EngineStringStringTests {
  implicit override def toEngineFromTests[R](e: Engine) = e.asInstanceOf[EngineFromTestsImpl]

  "An engine" should "throw UndecidedException if no code has been given" in {
    val e = builderWithUseCase.build
    evaluating { e("x") } should produce[UndecidedException]
    assertEngineMatches(e, Left(CodeAndScenarios(e.defaultRoot.left.get, List(), true)))
  }

  //tested elsewhere...but it's nice to keep the text ihere
  it should "replace the UndecidedException with the first scenario if no because" in {
    val e = builderWithUseCase.scenario("A").expected("Z").build
    assertEquals("Z", e("A"))
    assertEquals("Z", e("x"))
    val s = e.tests(0)
    assertEngineMatches(e, Left(CodeAndScenarios(s.actualCode, List(s))))
  }

  it should "move the default code to the else if first scenario has a because" in {
    val e = builderWithUseCase.scenario("A").because("A").expected("Z").build
    assertEquals("Z", e("A"))
    evaluating { e("x") } should produce[UndecidedException]

    val s = e.tests(0)
    assertEngineMatches(e, Right(EngineNode(inputs = List("A"),
      because = List("A"),
      yes = Left(CodeAndScenarios(s.actualCode, List(s))),
      no = Left(CodeAndScenarios(e.defaultRoot.left.get, List(), true)),
      scenarioThatCausedNode = s)))
  }

  it should "throw Exception if second scenario has because clause that matches first" in {
    val bldr = builderWithUseCase.
      scenario("A").expected("X").
      scenario("AB").because("A").expected("Y");
    val e = evaluating { bldr.build } should produce[ScenarioConflictException]
  }

  it should "throw exception if default condition matches because" in {
    val bldr = builderWithUseCase.
      scenario("C").expected("X").
      scenario("B").because("B").expected("Y").
      scenario("C").because("C").expected("Z")
    val e = evaluating { bldr.build } should produce[ScenarioConflictException]
    val defaultScenario = firstScenario(bldr.builderData)
    val lastScenario = allScenariosForBuild(bldr)(2)
    assertEquals(defaultScenario, e.scenario)
  }

  it should "Allow a default value to be specified" in {
    val e = builder.
      code((s: String) => "default").
      build;
    assertEquals("default", e("X"))
  }

  it should "Allow a default value to be specified with scenarios" in {
    val e = builder.code((s: String) => "default").
      useCase("UC1").
      scenario("A").expected("X").because("A").
      scenario("AB").because("B").expected("Y").
      build
    assertEquals("default", e("X"))
    assertEquals("X", e("A"))
    assertEquals("X", e("AQ"))
    assertEquals("Y", e("AB"))
  }

  it should "Allow throw exception if second default value is specified" in {
    val bldr = builder.code((s: String) => "default")
    val e = evaluating { bldr.code((s: String) => "default2") } should produce[CannotDefineCodeTwiceException]
    assertEquals("Original Code:\nCodeFn(((s: String) => \"default\"))\nBeingAdded\nCodeFn(((s: String) => \"default2\"))", e.getMessage())
  }

  "An engine with child engines" should "use the engine default code, if the child engine code isn't specified" in {
    val e = Engine.foldList[Int, String].code((x: Int) => "Engine" + x).
      childEngine("").
      build

    assertEquals(List("Engine1"), e(1))
  }
  "An engine with child engines" should "use the childengine default code" in {
    val e = Engine.foldList[Int, String].
      childEngine("").code((x: Int) => "ChildEngine" + x).
      build

    assertEquals(List("ChildEngine1"), e(1))
  }
  //    assertEquals("X", e("A"))
  //    assertEquals("Y", e("B"))
  //    assertEquals("Z", e("C"))
  //    assertEquals("X", e("D"))
  //    println(e)

}