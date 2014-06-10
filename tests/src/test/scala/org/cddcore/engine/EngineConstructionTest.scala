package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.engine.builder._

abstract class EngineConstructionTest[Params, R, B <: Builder[Params, R, R, B, E], E <: EngineTools[Params, R]] extends BuilderTest[Params, R, R, B, E] with DecisionTreeBuilderForTests[Params, R] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[Params, R]]
  implicit def toEngineFromTests[Params, R](x: EngineTools[Params, R]) = x.asInstanceOf[EngineFromTests[Params, R]]

  implicit def toSome[X](x: X) = Some(x)

  s"A blank $builderName " should "just have the default root" in {
    val engine = build
    val root = engine.asInstanceOf[EngineFromTests[Params, R]].tree.root
    assertEquals(defaultRoot, root)
  }

  builderName should "throw  CannotDefineTitleTwiceException if the title has already been set" in {
    scenario("A")
    update(_.title("X"))
    evaluating { update(_.title("X")) } should produce[CannotDefineTitleTwiceException]
  }

  it should "use the code of the engine to replace the 'default' throw undecided exception" in {
    code("A")
    val e = build
    assertEquals(result("A"), e.applyParams(params("a")))
  }

  it should "allow the 'because with string' to specify the description in the because clause" in {
    scenario("A"); expected("X")
    because("A", "some description")
    val e = build
    import ReportableHelper._
    val s = e.asRequirement.scenarios(0)
    val b = s.because
    assertEquals("some description", b.get.description)
  } 

  it should "put the tree into the engine description when built" in {
    val e = build
    val ed = e.asRequirement.asInstanceOf[EngineDescription[Params, R]]
    assertEquals(Some(e.tree), ed.tree)
  }

  it should "throw  CannotDefineDescriptionTwiceException if the description has already been set" in {
    scenario("A")
    update(_.description("X"))
    evaluating { update(_.description("X")) } should produce[CannotDefineDescriptionTwiceException]
  }
  it should "throw  CannotDefinePriorityTwiceException if the priority has already been set" in {
    scenario("A")
    update(_.priority(1))
    evaluating { update(_.priority(1)) } should produce[CannotDefinePriorityTwiceException]
  }
  it should "throw  CannotDefineBecauseTwiceException if the because has already been set" in {
    scenario("A")
    because("A")
    evaluating { because("A") } should produce[CannotDefineBecauseTwiceException]
  }
  it should "throw 'NeedScenarioExceptions'" in {
    evaluating { because("A") } should produce[NeedScenarioException]
    evaluating { matchOn("A", result("X")) } should produce[NeedScenarioException]
  }
  it should "throw BecauseClauseException if an exception is thrown by the because" in {
    val e = new RuntimeException
    scenario("A")

    val actual = evaluating { becauseException(e) } should produce[BecauseClauseScenarioException]
    assertEquals(e, actual.getCause())
  }

  it should "throw  CannotDefineExpectedTwiceException if the expected has already been set" in {
    scenario("A")
    expected("X")
    evaluating { expected("X") } should produce[CannotDefineExpectedTwiceException]
  }
  it should "throw  CannotDefineCodeTwiceException if the code has already been set" in {
    scenario("A")
    code("X")
    evaluating { code("X") } should produce[CannotDefineCodeTwiceException]
  }

  it should "throw ScenarioBecauseException  if the because is not true in the scenario" in {
    scenario("A")
    evaluating { because("B") } should produce[ScenarioBecauseException]

  }

  "An builder that hasn't been set up as a folding builder" should "throw CannotHaveChildEnginesWithoutFolderException" in {
    evaluating { update(_.childEngine("some title")) } should produce[CannotHaveChildEnginesWithoutFolderException]
  }

}

abstract class EngineConstruction1Test[P, R] extends EngineConstructionTest[P, R,  Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineConstruction2Test[P1, P2, R] extends EngineConstructionTest[(P1, P2),  R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineConstruction3Test[P1, P2, P3, R] extends EngineConstructionTest[(P1, P2, P3),  R,  Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringTest extends EngineConstruction1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringStringTest extends EngineConstruction2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringStringStringTest extends EngineConstruction3Test[String, String, String, String] with StringStringStringStringTest
