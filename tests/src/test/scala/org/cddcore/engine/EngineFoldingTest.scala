package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.engine.builder._
import ReportableHelper._
abstract class EngineFoldingTest[Params, R, FullR, B <: Builder[Params, R, FullR, B, E], E <: EngineTools[Params, R]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, R, FullR, B, E] with FoldingBuilderTest[R, FullR] {
  implicit def toSome[X](x: X) = Some(x)
  implicit def toFoldingEngine(e: EngineTools[Params, R]) = e.asInstanceOf[FoldingEngine[Params, R, FullR]]
  type FD = FoldingEngineDescription[Params, R, FullR]
  type ED = EngineDescription[Params, R]
  implicit def toFullR(seed: String): FullR
  implicit def toR(seed: String): R = result(seed)

  def compareFoldingEngineDescriptions(left: FD, nodes: List[BuilderNode[Params, R]]) = {
    assertEquals(nodes.size, 1)
    val right = nodes.head.asInstanceOf[FD]
    val withDifferentFoldingStuff = left.copy(foldingFn = right.foldingFn, initialValue = right.initialValue)
    assertEquals(withDifferentFoldingStuff, right)
  }

  "A folding builder" should "have a folding engine description with engine description when created" in {
    val b = currentBuilder
    val empty: FD = FoldingEngineDescription(initialValue = () => initialValue, foldingFn = foldingFn)
    compareFoldingEngineDescriptions(empty, currentBuilder.nodes)

  }
  it should "add the child engines under the main engine" in {
    update(_.childEngine("ce1").useCase("uc11").useCase("uc12"))
    update(_.childEngine("ce2").useCase("uc21").useCase("uc22"))
    update(_.childEngine("ce3").useCase("uc31").useCase("uc32"))

    val ce1 = EngineDescription[Params, R](title = "ce1", nodes = List(UseCase(title = "uc12"), UseCase(title = "uc11")))
    val ce2 = EngineDescription[Params, R](title = "ce2", nodes = List(UseCase(title = "uc22"), UseCase(title = "uc21")))
    val ce3 = EngineDescription[Params, R](title = "ce3", nodes = List(UseCase(title = "uc32"), UseCase(title = "uc31")))
    val fe = FoldingEngineDescription[Params, R, FullR](nodes = List(ce3, ce2, ce1), foldingFn = foldingFn, initialValue = () => initialValue)
    val actualFr = currentBuilder.nodes.head.asInstanceOf[FoldingEngineDescription[Params, R, FullR]]
    val actualCe3 = actualFr.nodes.head.asInstanceOf[EngineDescription[Params, R]]
    val actualUC32 = actualCe3.nodes.head
    val withDifferentFoldingStuff = actualFr.copy(foldingFn = fe.foldingFn, initialValue = fe.initialValue)
    assertEquals(UseCase[Params, R](title = "uc32"), actualUC32)
    assertEquals(ce3, actualCe3)
    assertEquals(fe.nodes, withDifferentFoldingStuff.nodes)
    assertEquals(fe, withDifferentFoldingStuff)
  }

  it should "create a folding engine with child engines" in {
    update(_.childEngine("ce1"))
    scenario("A")
    expected("X")
    update(_.childEngine("ce2"))
    scenario("B")
    expected("Y")
    val e = build

    val expectedValue = List[R]("X", "Y").foldLeft(initialValue)(foldingFn)
    assertEquals(expectedValue, e.applyParams(params("a")))
  }

  it should "throw an Exception if no child engines are specified" in {
    evaluating { build } should produce[CannotHaveFoldingEngineWithoutChildEnginesException]
  }

  it should "'modify children for build' prior to the build" in {
    code("Q"); expected("Y"); update(_.priority(1))
    update(_.childEngine("ce1")); update(_.priority(2))
    scenario("A"); expected("X"); code("X"); because("A")
    scenario("B"); expected("Q"); update(_.priority(3))
    update(_.childEngine("ce2")); code("W")
    scenario("C"); expected("X"); code("X"); because("C")
    scenario("D");
    val scenarios = Engine.test { build }.asRequirement.scenarios //not worried if it builds...
    val sa = scenarios(0)
    val sb = scenarios(1)
    val sc = scenarios(2)
    val sd = scenarios(3)
    assertEquals(s("A", code = resultCodeHolder("X"), expected = "X", because = "A", priority = 2), sa)
    assertEquals(s("B", code = resultCodeHolder("Q"), expected = "Q", priority = 3), sb)
    assertEquals(s("C", code = resultCodeHolder("X"), expected = "X", because = "C", priority = 1), sc)
    assertEquals(s("D", code = resultCodeHolder("W"), expected = "Y", priority = 1), sd)
  }
  it should "allow scenario to use default code in folding engine" in {
    code("Q"); update(_.childEngine("ce1"))
    scenario("A"); expected("X"); code("X"); because("A")
    scenario("B"); expected("Q")

    val scenarios = currentBuilder.all(classOf[Scenario[_, _]])
    val sb = scenarios(0)
    val sa = scenarios(1)
    assertEquals(s("A", code = resultCodeHolder("X"), expected = "X", because = "A"), sa)
    assertEquals(s("B", expected = "Q"), sb)

    val modified = Engine.test { build }.asRequirement.scenarios
    val sModifieda = modified(0)
    val sModifiedb = modified(1)
    assertEquals(s("A", code = resultCodeHolder("X"), expected = "X", because = "A"), sModifieda)
    assertEquals(s("B", code = resultCodeHolder("Q"), expected = "Q"), sModifiedb)

    val e = build
    assertEquals("_X", e.applyParams(params("A")))
    assertEquals("_X", e.applyParams(params("AB")))
    assertEquals("_Q", e.applyParams(params("B")))
  }

  it should "allow the same scenario in multiple engine without throwing a duplicate scenario exception" in {
    update(_.childEngine("ce1"))
    scenario("A")
    expected("X")
    update(_.childEngine("ce2"))
    scenario("A")
    expected("X")
    val e = build

    val expectedValue = List[R]("X", "X").foldLeft(initialValue)(foldingFn)
    assertEquals(expectedValue, e.applyParams(params("a")))
  }

  it should "add the references to the childengines" in {
    val doc = new Document(Some("doc"))
    val ref1 = Reference("ref1")
    val ref2 = Reference("ref2", doc)
    update(_.childEngine("").reference("ref1").reference("ref2", doc))
    val e = build.asInstanceOf[FoldingEngine[Params, R, FullR]]
    val ce = e.engines.head
    import ReportableHelper._
    assertEquals(Set(ref1, ref2), ce.asRequirement.references)
    assertEquals(List(doc), e.asRequirement.documents)
  }

  it should "have it's scenarios in text order" in {
    update(_.childEngine("ce1"))
    scenario("A"); expected("1")
    scenario("B"); expected("1")
    update(_.childEngine("ce2"))
    scenario("C"); expected("3")
    scenario("D"); expected("3")
    val e = build
    assertEquals(List("A", "B", "C", "D").map(params(_)), e.asRequirement.scenarios.map(_.params))
  }

  it should "have childEngines with requirements that are identical to the folding engine's requirements" in {
    update(_.childEngine("ce1"))
    scenario("A"); expected("1")
    scenario("B"); expected("1")
    update(_.childEngine("ce2"))
    scenario("C"); expected("3")
    scenario("D"); expected("3")
    val f = build
    val fs_ed = f.asRequirement.nodes
    val es_ed = f.engines.map(_.asRequirement)
    for ((f, e) <- fs_ed.zip(es_ed)) {
      if (!f.eq(e))
        if (f == e)
          fail(s"Equal but not identical\nFrom folding: $f\nFrom child:  $e")
        else
          fail(s"\nFrom folding: $f\nFrom child:    $e")
    }

  }

  it should "have a buildExceptions that is the aggregate of the child engine's when executed in test mode" in {
    val e0 = new RuntimeException("e0");
    val e1 = new RuntimeException("e1");
    val e = Engine.test {
      update(_.childEngine("ce1"))
      scenario("A")
      expected("Y")
      becauseException(e0)
      update(_.childEngine("ce2"))
      scenario("B")
      expected("Y")
      becauseException(e1)

      build
    }

    val exceptionMap = e.buildExceptions
    val sa = e.asRequirement.scenarios(0)
    val sb = e.asRequirement.scenarios(1)
    assertEquals(params("A"), sa.params)
    val mapToListOfClasses = exceptionMap.map.mapValues((listE) => listE.map(_.getClass))
    val mapToListOfExceptions = exceptionMap.map.mapValues((listE) => listE.map(_.getCause))
    val clazz = classOf[BecauseClauseScenarioException]
    assertEquals(Map(sa.textOrder -> List(clazz), sb.textOrder -> List(clazz)), mapToListOfClasses)
    assertEquals(Map(sa.textOrder -> List(e0), sb.textOrder -> List(e1)), mapToListOfExceptions)
  }
}

abstract class EngineFolding1Test[P, R, FullR]
  extends EngineFoldingTest[P, R,  FullR, Builder1[P, R, FullR], Engine1[P, R, FullR]]
  with FoldingBuilder1Test[P, R, FullR] {
  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P, R, FullR](foldingFn, initialValue) }
}
abstract class EngineFolding2Test[P1, P2, R, FullR] extends EngineFoldingTest[(P1, P2),  R, FullR, Builder2[P1, P2, R, FullR], Engine2[P1, P2, R, FullR]]
  with Builder2Test[P1, P2, R, FullR]
  with FoldingBuilder2Test[P1, P2, R, FullR] {
  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P1, P2, R, FullR](foldingFn, initialValue) }

}
abstract class EngineFolding3Test[P1, P2, P3, R, FullR] extends EngineFoldingTest[(P1, P2, P3),  R,  FullR, Builder3[P1, P2, P3, R, FullR], Engine3[P1, P2, P3, R, FullR]]
  with Builder3Test[P1, P2, P3, R, FullR]
  with FoldingBuilder3Test[P1, P2, P3, R, FullR] {
  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P1, P2, P3, R, FullR](foldingFn, initialValue) }
}

trait StringToStringFoldingTest {
  def foldingFn = (x: String, y: String) => { x + y }
  def initialValue: String = "_"
  implicit def toFullR(x: String) = x
}

@RunWith(classOf[JUnitRunner])
class EngineFoldingStringStringTest extends EngineFolding1Test[String, String, String] with StringStringTest with StringToStringFoldingTest

@RunWith(classOf[JUnitRunner])
class EngineFoldingStringStringStringTest extends EngineFolding2Test[String, String, String, String] with StringStringStringTest with StringToStringFoldingTest

@RunWith(classOf[JUnitRunner])
class EngineFoldingStringStringStringStringTest extends EngineFolding3Test[String, String, String, String, String] with StringStringStringStringTest with StringToStringFoldingTest
