package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

case class Holder(var value: String);

@RunWith(classOf[JUnitRunner])
class EngineMutabilityTests extends EngineString1Tests[Holder] {
  val mutable = Holder("A");
  val becauseA = new Because[B]((h => h.value contains "A"), "hA");
  val becauseB = new Because[B]((h => h.value contains "B"), "hB");
  val becauseAB = new Because[B]((h => (h.value contains "A") & (h.value contains "B")), "hAB");

  val builderWithScenario = builderWithUseCase.scenario(mutable).configuration((h: Holder) => h.value = "W")
  val builderWithDefault = builderWithScenario.expected("Z");
  val engineWithDefault = builderWithDefault.build
  val defaultScenario = engineWithDefault.tests.head

  def addW(e: RealScenarioBuilder): RealScenarioBuilder = e.scenario(mutable).expected("XW").configuration((h) => h.value = "W")
  def addA(e: RealScenarioBuilder): RealScenarioBuilder = e.scenario(mutable).expected("XA").because(becauseA).configuration((h) => h.value = "A")
  def addB(e: RealScenarioBuilder): RealScenarioBuilder = e.scenario(mutable).expected("XB").because(becauseB).configuration((h) => h.value = "B")
  def addAB(e: RealScenarioBuilder): RealScenarioBuilder = e.scenario(mutable).expected("XAB").because(becauseAB).configuration((h) => h.value = "AB")

  val bldr: RealScenarioBuilder = addAB(addB(addA(addW(builderWithUseCase))))

  "A scenario with a configurer" should "change the configured items" in {
    val e = bldr.build
    val s = e.tests
    val w = s(0);
    val a = s(1)
    val b = s(2)
    val ab = s(3);

    w.configure; assert(mutable.value == "W")
    a.configure; assert(mutable.value == "A")
    b.configure; assert(mutable.value == "B")
    ab.configure; assert(mutable.value == "AB")
  }

  "An engine passed a mutable parameter" should "reset the paramaters prior to checking the because 1" in {
    val _w: RealScenarioBuilder = addW(builderWithUseCase)
    val _w_a: RealScenarioBuilder = addA(_w)
    val _w_b: RealScenarioBuilder = addB(_w)
    val _w_ab: RealScenarioBuilder = addAB(_w)

    val _w_a_b: RealScenarioBuilder = addB(_w_a)
    val _w_a_ab: RealScenarioBuilder = addAB(_w_a)

    val _w_b_a: RealScenarioBuilder = addA(_w_b)
    val _w_b_ab: RealScenarioBuilder = addAB(_w_b)

    val _w_ab_a: RealScenarioBuilder = addA(_w_ab)
    val _w_ab_b: RealScenarioBuilder = addB(_w_ab)

    val w_a_b_ab = addAB(_w_a_b)
    val _w_a_ab_b = addB(_w_a_ab)
    val w_b_a_ab = addAB(_w_b_a)
    val w_b_ab_a = addA(_w_b_ab)

    _w.build.validateScenarios
    _w_a.build.validateScenarios
    _w_b.build.validateScenarios
    _w_ab.build.validateScenarios
    _w_a_b.build.validateScenarios
    _w_a_ab.build.validateScenarios
    _w_b_a.build.validateScenarios
    _w_b_ab.build.validateScenarios
    _w_ab_a.build.validateScenarios
    _w_ab_b.build.validateScenarios
    w_a_b_ab.build.validateScenarios
    _w_a_ab_b.build.validateScenarios
    w_b_a_ab.build.validateScenarios
    w_b_ab_a.build.validateScenarios
  }

}