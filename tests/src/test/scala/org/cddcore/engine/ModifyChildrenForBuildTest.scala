package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

object ModifyChildrenForBuildTest

abstract class ModifyChildrenForBuildTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[Params, BFn, R, RFn]]
  implicit def toSome[X](x: X) = Some(x)
  "Scenarios when modifiedForBuild" should "inherit priority from parent if not defined" in {
    update(_.priority(2).useCase("UC").priority(1))
    scenario("A")
    assertEquals(EngineDescription[Params, BFn, R, RFn](priority = 2, nodes = List(
      UseCase(title = "UC", priority = 1, nodes = List(s("A", priority = 1))))),
      modifiedChildrenForBuild)
  }
  it should "use own priority if defined" in {
    update(_.priority(2).useCase("UC").priority(1))
    scenario("A")
    update(_.priority(3))
    assertEquals(EngineDescription[Params, BFn, R, RFn](priority = 2, nodes = List(
      UseCase(title = "UC", priority = 1, nodes = List(
        s("A", priority = 3))))),
      modifiedChildrenForBuild)
  }
  it should "inherit expected from parent if not defined" in {
    update(_.expected(result("X")).useCase("UC").expected(result("Y")))
    scenario("A")
    assertEquals(EngineDescription[Params, BFn, R, RFn](expected = Right(result("X")), nodes = List(
      UseCase(title = "UC", expected = Right(result("Y")), nodes = List(s("A", expected = "Y"))))),
      modifiedChildrenForBuild)
  }

  it should "use own  expected if  defined" in {
    update(_.expected(result("X")).useCase("UC").expected(result("Y")))
    scenario("A")
    update(_.expected(result("Z")))
    assertEquals(EngineDescription[Params, BFn, R, RFn](expected = Right(result("X")), nodes = List(
      UseCase(title = "UC", expected = Right(result("Y")), nodes = List(
        s("A", expected = "Z"))))),
      modifiedChildrenForBuild)
  }
  it should "inherit code from parent if not defined" in {
    code("X")
    update(_.useCase("UC"))
    code("Y")
    scenario("A")

    assertEquals(EngineDescription[Params, BFn, R, RFn](code = resultCodeHolder("X"), nodes = List(
      UseCase(title = "UC", code = resultCodeHolder("Y"), nodes = List(
        s("A", code = resultCodeHolder("Y")))))),
      modifiedChildrenForBuild)
  }

  it should "use own  code if  defined" in {
    implicit def toEngineFromTests[Params, BFn, R, RFn](x: EngineTools[Params, BFn, R, RFn]) = x.asInstanceOf[EngineFromTests[Params, BFn, R, RFn]]

    code("X")
    update(_.useCase("UC")); code("Y")
    scenario("A"); code("Z"); expected("Z")

    val m = modifiedChildrenForBuild
    
    assertEquals(EngineDescription[Params, BFn, R, RFn](code = resultCodeHolder("X"), nodes = List(
      UseCase(title = "UC", code = resultCodeHolder("Y"), nodes = List(
        s("A", code = resultCodeHolder("Z"), expected="Z"))))),
      modifiedChildrenForBuild)
    val e = build

    assertEquals(result("Z"), e.applyParams(params("A")))
    assertEquals(result("Z"), e.applyParams(params("B")))
  }

  it should "sort children by text order" in {
    update(_.useCase("UC1").priority(1).useCase("UC2").priority(2).useCase("UC3").useCase("UC4"))
    assertEquals(EngineDescription[Params, BFn, R, RFn](nodes = List(
      UseCase(title = "UC1", priority = 1),
      UseCase(title = "UC2", priority = 2),
      UseCase(title = "UC3"),
      UseCase(title = "UC4"))),
      modifiedChildrenForBuild)
  }

  "A builder" should "create an engine with the modified requirements as it's requirements" in {
    update(_.priority(2).useCase("UC").priority(1))
    expected("X")
    scenario("A")
    val e = build
    val actual = e.asRequirement.asInstanceOf[EngineDescription[Params, BFn, R, RFn]]
    val expect = EngineDescription[Params, BFn, R, RFn](priority = 2, nodes = List(UseCase(title = "UC", priority = 1, expected = Right(result("X")),
      nodes = List(s("A", priority = 1, expected = "X")))))
    assertEquals(expect, actual.copy(tree = None)) // the tree isn't part of this test
  }

}

abstract class ModifyChildrenForBuild1Test[P, R] extends ModifyChildrenForBuildTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class ModifyChildrenForBuild2Test[P1, P2, R] extends ModifyChildrenForBuildTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class ModifyChildrenForBuild3Test[P1, P2, P3, R] extends ModifyChildrenForBuildTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class ModifyChildrenForBuildStringStringTest extends ModifyChildrenForBuild1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class ModifyChildrenForBuildStringStringStringTest extends ModifyChildrenForBuild2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class ModifyChildrenForBuildStringStringStringStringTest extends ModifyChildrenForBuild3Test[String, String, String, String] with StringStringStringStringTest
