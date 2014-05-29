package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import org.scalatest.junit.JUnitRunner

object AbstractTestTest {
  def main(args: Array[String]) {

  }
}

abstract class AbstractTestTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {

  "The abstractTest class" should "allow becauses to be specified and the because means 'all these letters are in the result'" in {

    def checkBecause(because: Seed, p: Seed, expected: Boolean) {
      val bfn = becauseBfn("ABC")
      val bc = buildEngine.mc.makeBecauseClosure(params(p))
      assertEquals(expected, bc(bfn))
    }
    checkBecause("ABC", "A", false)
    checkBecause("ABC", "A", false)
    checkBecause("ABC", "A", false)
    checkBecause("ABC", "ABC", true)
    checkBecause("ABC", "ABCD", true)
    checkBecause("ABC", "CABD", true)
  }
}

abstract class AbstractTest1Test[P, R] extends AbstractTestTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class AbstractTest2Test[P1, P2, R] extends AbstractTestTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class AbstractTest3Test[P1, P2, P3, R] extends AbstractTestTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class AbstractTestStringStringTest extends AbstractTest1Test[String, String] with StringStringTest {
  def apply(p: String): String = ???
}

@RunWith(classOf[JUnitRunner])
class AbstractTestStringStringStringTest extends AbstractTest2Test[String, String, String] with StringStringStringTest {
  def apply(p1: String, p2: String): String = ???
}

@RunWith(classOf[JUnitRunner])
class AbstractTestStringStringStringStringTest extends AbstractTest3Test[String, String, String, String] with StringStringStringStringTest {
  def apply(p1: String, p2: String, p3: String): String = ???
}