package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class BuilderLensTest[Params, R, B <: Builder[Params, R, R, B, E], E <: EngineTools[Params, R]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, R, R, B, E] {
  implicit def toSome[X](x: X) = Some(x)

  val s0 = s("0")
  val s1 = s("1")

  val uc = UseCase[Params, R]()
  val ucS0 = UseCase[Params, R](nodes = List(s0))
  val ucS1 = UseCase[Params, R](nodes = List(s1))
  val uc1 = UseCase[Params, R](title = Some("UC1"))

  val ed = EngineDescription[Params, R]()
  val ed1 = EngineDescription[Params, R](title = Some("ED1"))
  val edUc = EngineDescription[Params, R](nodes = List(uc))
  val edUc1 = EngineDescription[Params, R](nodes = List(uc1))
  val edUcS0 = EngineDescription[Params, R](nodes = List(ucS0))
  val edUcS1 = EngineDescription[Params, R](nodes = List(ucS1))
  val edUc1UcS0 = EngineDescription[Params, R](nodes = List(ucS0, uc))
  val edUc1UcS1 = EngineDescription[Params, R](nodes = List(ucS1, uc))

  val builderEd = initializeBuilder(List(ed))
  val builderEd1 = initializeBuilder(List(ed1))
  val builderEdUc = initializeBuilder(List(edUc))
  val builderEdUc1 = initializeBuilder(List(edUc1))
  val builderEdUcS0 = initializeBuilder(List(edUcS0))
  val builderEdUcS1 = initializeBuilder(List(edUcS1))
  val builderEdUc1UcS0 = initializeBuilder(List(edUc1UcS0))
  val builderEdUc1UcS1 = initializeBuilder(List(edUc1UcS1))
  val lens = currentBuilder.bl

  s"$builderName with currentNodeL" should "focus on the engine description if no deeper nodes" in {
    assertEquals(ed, lens.currentNodeL.get(builderEd))
    assertEquals(ed1, lens.currentNodeL.get(builderEd1))

    val actual = lens.currentNodeL.set(builderEd, ed1)
    val expected = builderEd1
    assertEquals(expected, actual)
  }

  it should "focus on the deepest child... checking builder / enginedescription / usecase" in {
    assertEquals(uc, lens.currentNodeL.get(builderEdUc))
    assertEquals(uc1, lens.currentNodeL.get(builderEdUc1))

    assertEquals(builderEdUc1, lens.currentNodeL.set(builderEdUc, uc1))
  }

  it should "focus on the deepest child... checking builder / enginedescription / usecase (two of) / scenario" in {
    assertEquals(s0, lens.currentNodeL.get(builderEdUcS0))
    assertEquals(s1, lens.currentNodeL.get(builderEdUcS1))
    assertEquals(s0, lens.currentNodeL.get(builderEdUc1UcS0))
    assertEquals(s1, lens.currentNodeL.get(builderEdUc1UcS1))

    assertEquals(builderEdUcS1, lens.currentNodeL.set(builderEdUcS0, s1))
    assertEquals(builderEdUc1UcS1, lens.currentNodeL.set(builderEdUc1UcS0, s1))
  }

  s"$builderName with nextUseCaseHolderL" should "focus on the engine description if no deeper nodes" in {
    assertEquals(ed, lens.nextUseCaseHolderL.get(builderEd))
    assertEquals(ed1, lens.nextUseCaseHolderL.get(builderEd1))

    assertEquals(builderEd1, lens.nextUseCaseHolderL.set(builderEd, ed1))
  }

  it should "focus on the engine description if a use case is the bottom item" in {
    assertEquals(edUc, lens.nextUseCaseHolderL.get(builderEdUc))

    assertEquals(builderEd1, lens.nextUseCaseHolderL.set(builderEdUc, ed1))
  }
  it should "focus on the engine description if ed / usecase / scenario is the bottom item" in {
    assertEquals(edUc1UcS0, lens.nextUseCaseHolderL.get(builderEdUc1UcS0))

    assertEquals(builderEd1, lens.nextUseCaseHolderL.set(builderEdUc1UcS1, ed1))
  }
  s"$builderName with nextScenarioHolderL" should "focus on the engine description if no deeper nodes" in {
    assertEquals(ed, lens.nextScenarioHolderL.get(builderEd))
    assertEquals(ed1, lens.nextScenarioHolderL.get(builderEd1))

    assertEquals(builderEd1, lens.nextScenarioHolderL.set(builderEd, ed1))
  }

  it should "focus on a use case is a use case is the bottom item" in {
    assertEquals(uc, lens.nextScenarioHolderL.get(builderEdUc))
    assertEquals(uc1, lens.nextScenarioHolderL.get(builderEdUc1))

    assertEquals(builderEdUc1, lens.nextScenarioHolderL.set(builderEdUc, uc1))
  }
  it should "focus on a use case if a scenario under a use case is the bottom item" in {
    assertEquals(ucS0, lens.nextScenarioHolderL.get(builderEdUcS0))

    assertEquals(builderEdUc1, lens.nextScenarioHolderL.set(builderEdUcS0, uc1))
  }

}

abstract class Lens1Test[P, R] extends BuilderLensTest[P, R,  Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class Lens2Test[P1, P2, R] extends BuilderLensTest[(P1, P2),  R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class Lens3Test[P1, P2, P3, R] extends BuilderLensTest[(P1, P2, P3),  R,  Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class LensStringStringTest extends Lens1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class LensStringStringStringTest extends Lens2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class LensStringStringStringStringTest extends Lens3Test[String, String, String, String] with StringStringStringStringTest

object BuilderLensTest {
  def main(args: Array[String]) {
  }
}