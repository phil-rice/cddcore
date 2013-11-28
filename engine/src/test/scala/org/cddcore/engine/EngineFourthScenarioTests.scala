package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EngineThreeScenarioTests extends EngineStringStringTests {
 implicit override  def toEngineFromTests[R](e: Engine) = e.asInstanceOf[EngineFromTestsImpl]


  val builderA = builderWithDefault.scenario("A").because(becauseA).expected("A")
  val builderB = builderWithDefault.scenario("B").because(becauseB).expected("B")
  val builderAB = builderWithDefault.scenario("AB").because(becauseAB).expected("AB")

  val builderA_B = builderA.scenario("B").because(becauseB).expected("A_B")
  val builderA_AB = builderA.scenario("AB").because(becauseAB).expected("A_AB")

  val builderB_A = builderB.scenario("A").because(becauseA).expected("B_A")
  val builderB_AB = builderB.scenario("AB").because(becauseAB).expected("B_AB")

  val builderAB_A = builderAB.scenario("A").because(becauseA).expected("AB_A");
  val builderAB_B = builderAB.scenario("B").because(becauseB).expected("AB_B");

  val builderA_B_AB = builderA_B.scenario("AB").because(becauseAB).expected("A_B_AB")
  val builderA_AB_B = builderA_AB.scenario("B").because(becauseB).expected("A_AB_B")

  val builderB_A_AB = builderB_A.scenario("AB").because(becauseAB).expected("B_A_AB")
  val builderB_AB_A = builderB_A.scenario("AB").because(becauseA).expected("B_AB_A")

  val builderAB_A_B = builderAB.scenario("B").because(becauseB).expected("AB_A_B");
  val builderAB_B_A = builderAB.scenario("A").because(becauseA).expected("AB_B_A");

  "An engine" should "allow any combination of two scenarios to work" in {
    builderA.build.validateScenarios
    builderB.build.validateScenarios
    builderAB.build.validateScenarios
  }
  
 it should "allow any combination of three scenarios to work" in {
    builderA_B.build.validateScenarios
    builderA_AB.build.validateScenarios
    builderB_A.build.validateScenarios
    builderB_AB.build.validateScenarios
  }
  
 it should "allow any combination of four scenarios to work" in {
    builderA_B_AB.build.validateScenarios
    builderA_AB_B.build.validateScenarios
    builderB_A_AB.build.validateScenarios
    builderB_AB_A.build.validateScenarios
    builderAB_A_B.build.validateScenarios
    builderAB_B_A.build.validateScenarios
  }
 

}