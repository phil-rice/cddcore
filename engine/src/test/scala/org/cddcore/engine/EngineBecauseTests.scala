package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineBecauseTests extends AbstractTest {

  "An exception in a because clause" should "throw a BecauseClauseException" in {
    val e = new RuntimeException
    val b = Engine[Int, String].scenario(0).expected("x").because((x: Int) => throw e)
    val bce = evaluating { b.build } should produce[BecauseClauseException]
    assertEquals(e, bce.getCause())

  }

  "An nested exception in a because clause" should "throw a BecauseClauseException" in {
    val e = new RuntimeException
    val engine1 = Engine[Int, String].scenario(0).expectException(e).code((x: Int) => throw e).build
    val b = Engine[Int, String].scenario(0).expected("x").because((x: Int) => { engine1(x); true })
    val bce = evaluating { b.build } should produce[BecauseClauseException]
    assertEquals(e, bce.getCause())
  }

  "An undecided exception in a because clause" should "throw a BecauseClauseException" in {
    val engine1 = Engine[Int, String].build
    val b = Engine[Int, String].scenario(0).expected("x").because((x: Int) => { engine1(x); true })
    val bce = evaluating { b.build } should produce[BecauseClauseException]
    assert { bce.getCause().isInstanceOf[UndecidedException] }
  }

}