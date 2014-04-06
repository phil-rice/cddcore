package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DefaultCodeTests extends AbstractTest {

  "An engine" should "use the default code, if nothing else is specified " in {
    val e = Engine[Int, Int]().code((x: Int) => x * 2).build
    assertEquals(2, e(1))
    assertEquals(4, e(2))
  }

  it should "use the default code that would normally be undecided" in {
    val b = Engine[Int, Int]().
      code((x: Int) => x * 2).
      scenario(1).expected(2).
      scenario(2).expected(4).
      scenario(3).expected(3).code((c: Int) => c).because((x: Int) => x == 3).
      scenario(4).expected(8).
      scenario(5).expected(10)
    val e = b.build
    assertEquals(4, e(2))
    assertEquals(3, e(3))
  }

  "A childEngine" should "use the default code, if nothing else is specified" in {
    val e = Engine.foldList[Int, Int].code((x: Int) => 0).
      childEngine("Double, except three which returns itself").
      code((x: Int) => x * 2).
      scenario(1).expected(2).
      scenario(2).expected(4).
      scenario(3).expected(3).code((c: Int) => c).because((x: Int) => x == 3).
      scenario(4).expected(8).
      scenario(5).expected(10).
      build
    assertEquals(List(4), e(2))
    assertEquals(List(3), e(3))
  }

}