package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ChildEngineTests extends AbstractTest {

  "An engine with a child engine and no fold function" should "throw CannotHaveChildEnginesWithoutFolderException" in {
    val b1 = Engine[Int, String]().childEngine("ce1")
    evaluating { b1.build } should produce[CannotHaveChildEnginesWithoutFolderException]

    val b2 = Engine[Int, Int, String]().childEngine("ce2")
    evaluating { b2.build } should produce[CannotHaveChildEnginesWithoutFolderException]

    val b3 = Engine[Int, Int, Int, String]().childEngine("ce3")
    evaluating { b3.build } should produce[CannotHaveChildEnginesWithoutFolderException]

  }

  "An engine with a child engine" should "allow each child engine to come to correct conclusion" in {
    val childEngines = Engine[Int, String]().
      childEngine("ce0").scenario(0).expected("zero").
      childEngine("ce1").scenario(1).expected("one").
      builderData.childrenModifiedForBuild.collect { case e: ChildEngine[_] => e }.reverse

    assertEquals("zero", childEngines(0).applyParams(List(0)))
    assertEquals("zero", childEngines(0).applyParams(List(1)))

    assertEquals("one", childEngines(1).applyParams(List(0)))
    assertEquals("one", childEngines(1).applyParams(List(1)))
  }

  it should "use the fold function to produce the correct result" in {
    val b = Engine.folding[Int, String, String](_ + _, { "Init" }).
      childEngine("ce0").scenario(0).expected("Zero").
      childEngine("ce1").scenario(1).expected("One")
    val e = b.build
    assertEquals("InitZeroOne", e(0))
    assertEquals("InitZeroOne", e(123))
  }

}