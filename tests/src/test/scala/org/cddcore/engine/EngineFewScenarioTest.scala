package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class EngineFewScenarioTest[Params, R, B <: Builder[Params, R, R, B, E], E <: EngineTools[Params, R]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, R, R, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  implicit def toDecisionTreeDecisionTree[Params, R](x: EngineTools[Params, R]) = x.asInstanceOf[DecisionTree[Params, R]]

  def check(seeds: String*) {
    for (these <- seeds.toList.permutations) {
      resetBuilder
      try {
        for (s <- these) {
          try {
            scenario(s);
            update(_.expected(result(s)));
            because(s)
            val e = build
          } catch { case e: Exception => throw new RuntimeException("Current item" + s + " which is in " + these, e) }
        }
        build
      } catch { case e: Exception => throw new RuntimeException("These" + these, e) }
    }
  }

  builderName should "allow any combination of two scenarios to work" in {
    check("A", "AB")
    check("A", "B")
  }

  it should "allow any combination of three scenarios to work" in {
    check("A", "AB", "ABC")
    check("A", "B", "AB")
    check("A", "B", "C")
  }

  it should "allow any combination of four scenarios to work" in {
    check("A", "AB", "ABC", "ABCD")
    check("A", "B", "AB", "ABC")
    check("A", "B", "C", "ABC")
    check("A", "B", "AB", "ABC")
    check("A", "B", "C", "D")
  }
  it should "allow any combination of five scenarios to work" in {
    check("A", "AB", "ABC", "ABCD", "ABCDE")
    check("A", "B", "AB", "ABC", "D")
    check("A", "B", "C", "ABC", "BC")
    check("A", "B", "AB", "ABC", "BC")
    check("A", "B", "C", "D", "E")
  }

}

abstract class EngineFewScenario1Test[P, R] extends EngineFewScenarioTest[P, R,  Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineFewScenario2Test[P1, P2, R] extends EngineFewScenarioTest[(P1, P2),  R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineFewScenario3Test[P1, P2, P3, R] extends EngineFewScenarioTest[(P1, P2, P3),  R,  Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R,R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineFewScenarioStringStringTest extends EngineFewScenario1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineFewScenarioStringStringStringTest extends EngineFewScenario2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineFewScenarioStringStringStringStringTest extends EngineFewScenario3Test[String, String, String, String] with StringStringStringStringTest
