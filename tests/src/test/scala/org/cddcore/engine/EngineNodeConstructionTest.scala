package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class BuilderNodeConstructionTest[Params, BFn, R, RFn,  B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  val doc = Document()
  val ref1 = Reference("1", doc)
  val ref2 = Reference("2", doc)
  val ref3 = Reference("3", doc)
  val ref1None = Reference("1", None)
  val ref2None = Reference("2", None)
  val ref3None = Reference("3", None)

  builderName should "allow an empty engine to be made" in {
    val b = currentBuilder
    assertEquals(List(EngineDescription[Params,BFn,R, RFn]()), currentBuilder.nodes)
  }

  it should "allow the engine descriptions properties to be set" in {
    update((b) => b.title("EngineTitle").description("EngineDescription").priority(1).expected(result("X")))
    code("X")
    assertEquals(List(EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", description = "EngineDescription", code = resultCodeHolder("X"), priority = 1, expected = Right(result("X")))),
      currentBuilder.nodes)
  }

  it should "allow a use case to be added" in {
    update((b) => b.title("EngineTitle").useCase("useCase1").expected(result("X")))
    assertEquals(List(EngineDescription[Params,BFn,R, RFn](
      title = "EngineTitle",
      nodes = List(UseCase(title = "useCase1", expected = Right(result("X")))))), currentBuilder.nodes)
  }
  it should "allow a second use case to be added" in {
    update((b) => b.title("EngineTitle").useCase("useCase1").useCase("useCase2"))
    assertEquals(List(EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", nodes = List(UseCase(title = "useCase2"), UseCase(title = "useCase1")))), currentBuilder.nodes)
  }

  it should "allow a scenario to added under an engine" in {
    update((b) => b.title("EngineTitle"))
    scenario("A")
    assertEquals(List(EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", nodes = List(s("A")))), currentBuilder.nodes)

  }
  it should "allow a scenario to added under a usecase" in {
    update((b) => b.title("EngineTitle").useCase("useCase1"))
    scenario("A")
    assertEquals(List(EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", nodes = List(UseCase(title = "useCase1", nodes = List(s("A")))))), currentBuilder.nodes)
  }
  it should "allow a multiple scenarios to added under a usecase" in {
    update((b) => b.title("EngineTitle").useCase("useCase1"))
    scenario("A")
    scenario("B")
    scenario("C")
    assertEquals(List(EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", nodes = List(UseCase(title = "useCase1", nodes =
      List(s("C"), s("B"), s("A")))))), currentBuilder.nodes)
  }

  it should "allow references to be added" in {

    update((b) => b.title("EngineTitle").reference("1", doc).useCase("useCase1").reference("2", doc))
    scenario("A")
    update((b) => b.reference("3", doc))
    assertEquals(List(
      EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", references = Set(ref1), nodes = List(
        UseCase(title = "useCase1", references = Set(ref2), nodes = List(
          s("A", references = Set(ref3))))))), currentBuilder.nodes)
  }
  it should "allow references to be added when doc is not specified" in {

    update((b) => b.title("EngineTitle").reference("1").useCase("useCase1").reference("2"))
    scenario("A")
    update((b) => b.reference("3"))
    assertEquals(List(
      EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", references = Set(ref1None), nodes = List(
        UseCase(title = "useCase1", references = Set(ref2None), nodes = List(
          s("A", references = Set(ref3None))))))), currentBuilder.nodes)
  }

  it should "allow multiple references to be added" in {
    update((b) => b.title("EngineTitle").reference("1", doc).reference("2", doc).reference("3", doc))
    assertEquals(List(EngineDescription[Params,BFn,R, RFn](title = "EngineTitle", references = Set(ref1, ref2, ref3))), currentBuilder.nodes)
  }

  it should "allow code to be added" in {
    code("X")
    update(_.useCase("UC"))
    code("Y")
    scenario("A")
    code("Z")

    assertEquals(List(EngineDescription[Params,BFn,R, RFn](code = resultCodeHolder("X"), nodes = List(
      UseCase(title = "UC", code = resultCodeHolder("Y"), nodes = List(
        s("A", code = resultCodeHolder("Z"))))))),
      currentBuilder.nodes)
  }

  it should "allow because to be added" in {
    update(_.useCase("UC"))
    scenario("A")
    because("A")

    assertEquals(List(EngineDescription[Params,BFn,R, RFn](nodes = List(
      UseCase(title = "UC", nodes = List(
        s("A", because = "A")))))),
      currentBuilder.nodes)
  }

}

abstract class BuilderNodeConstruction1Test[P, R] extends BuilderNodeConstructionTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class BuilderNodeConstruction2Test[P1, P2, R] extends BuilderNodeConstructionTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class BuilderNodeConstruction3Test[P1, P2, P3, R] extends BuilderNodeConstructionTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R,R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class BuilderNodeConstructionStringStringTest extends BuilderNodeConstruction1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class BuilderNodeConstructionStringStringStringTest extends BuilderNodeConstruction2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class BuilderNodeConstructionStringStringStringStringTest extends BuilderNodeConstruction3Test[String, String, String, String] with StringStringStringStringTest

