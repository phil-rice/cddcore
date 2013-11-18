package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ChildEngineTests extends AbstractTest {

  "An engine with one child engine" should "behave like a normal engine" in {
    val e = Engine[Int, String]().childEngine("ce1").scenario(0, "zero").expected("def").build
    assertEquals("def", e(0))
    assertEquals("def", e(4))
  }

  "AN engine with two child engines and no fold function" should "throw CannotHaveChildEnginesWithoutFolderException" in {
    val b1 = Engine[Int, String]().
      childEngine("ce1").scenario(0, "zero").expected("zero").
      childEngine("ce2").scenario(1, "one").expected("one");
    val b2 = Engine[Int, Int, String]().
      childEngine("ce1").scenario(0, 0, "zero").expected("zero").
      childEngine("ce2").scenario(1, 1, "one").expected("one");
    val b3 = Engine[Int, Int, Int, String]().
      childEngine("ce1").scenario(0, 0, 0, "zero").expected("zero").
      childEngine("ce2").scenario(1, 1, 1, "one").expected("one");

    evaluating { b1.build } should produce[CannotHaveChildEnginesWithoutFolderException]
    evaluating { b2.build } should produce[CannotHaveChildEnginesWithoutFolderException]
    evaluating { b3.build } should produce[CannotHaveChildEnginesWithoutFolderException]
  }
  
  it should "allow each child engine to come to correct conclusion" in {
    
  }
  
}