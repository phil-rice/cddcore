package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BuilderExpectedTests extends AbstractTest {
  "A scenario" should "get its expected from it's antecendants if not defined" in {
    val e = Engine[Int, String].expected("engine").
      useCase("uc0").
      scenario(0).because((x: Int) => x == 0).
      useCase("uc1").expected("usecase").
      scenario(1).because((x: Int) => x == 1).
      useCase("uc2").
      scenario(2).because((x: Int) => x == 2).expected("scenario").
      build

    assertEquals("engine", e(0))
    assertEquals("usecase", e(1))
    assertEquals("scenario", e(2))
  }
  
  it should "get its expected from it's antecendants if not defined, when in a child engine" in {
    val b = Engine.foldList[Int, String].expected("engine").
      childEngine("ce0").expected("childEngine").
      useCase("uc00").
      scenario(0).because((x: Int) => x == 0).
      useCase("uc01").expected("usecase").
      scenario(1).because((x: Int) => x == 1).
      useCase("uc02").
      scenario(2).because((x: Int) => x == 2).expected("scenario")
    val e = b.build

    assertEquals(List("childEngine"), e(0))
    assertEquals(List("usecase"), e(1))
    assertEquals(List("scenario"), e(2))
  }

  it should "get its expected from it's antecendants if not defined, when in a child engine. Checking with two child engines" in {
    val e = Engine.foldList[Int, String].expected("engine").
      childEngine("ce0").expected("childEngine").
      useCase("uc00").
      scenario(0).because((x: Int) => x == 0).
      useCase("uc01").expected("usecase").
      scenario(1).because((x: Int) => x == 1).
      useCase("uc02").
      scenario(2).because((x: Int) => x == 2).expected("scenario").
      useCase("stuff left over...stops exception").
      scenario(3).because((x: Int) => x > 2).expected("stuff0").

      childEngine("ce1").
      scenario(3).because((x: Int) => x == 3).
      scenario(4).because((x: Int) => x == 4).expected("scenario").
      useCase("stuff left over...stops exception").
      scenario(2).because((x: Int) => x < 3).expected("stuff1").
      build

    assertEquals(List("stuff1", "childEngine"), e(0))
    assertEquals(List("stuff1", "usecase"), e(1))
    assertEquals(List("stuff1", "scenario"), e(2))
    assertEquals(List("engine", "stuff0"), e(3))
    assertEquals(List("scenario", "stuff0"), e(4))
  }

}