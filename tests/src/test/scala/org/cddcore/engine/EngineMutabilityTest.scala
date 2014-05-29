package org.cddcore.engine

import org.junit.runner.RunWith
import org.cddcore.engine.builder._
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

object Holder {
  val apply = new Holder("")
}
case class Holder(var value: String);

abstract class EngineMutabilityTest[Params, BFn, R, RFn,  B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  protected def resetMutableParams(holders: Params, s: String)
  def check(seeds: String*) {
    for (these <- seeds.toList.permutations) {
      resetBuilder
      try {
        for (s <- these) {
          try {
            scenario(s);
            configurate((params) => resetMutableParams(params, s))
            val b = currentBuilder.nodes.head
            update(_.expected(result(s)));
            val b1 = currentBuilder.nodes.head
            because(s)
            val b2 = currentBuilder.nodes.head
            val e = build
          } catch { case e: Exception => throw new RuntimeException("Current item" + s + " which is in " + these, e) }
        }
        build
      } catch { case e: Exception => throw new RuntimeException("These" + these, e) }
    }
  }
  "A scenario with a configurer" should "change the configured items" in {
    check("A")
    check("A", "B")
    check("A", "B", "AB")
    check("A", "B", "AB", "ABC")
  }

}

abstract class EngineMutability1Test[P, R] extends EngineMutabilityTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineMutability2Test[P1, P2, R] extends EngineMutabilityTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineMutability3Test[P1, P2, P3, R] extends EngineMutabilityTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R,R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineMutabilityHolderStringTest extends EngineMutability1Test[Holder, String] with HolderStringTest {
  protected def resetMutableParams(holder: Holder, s: String) = holder.value = s
}

@RunWith(classOf[JUnitRunner])
class EngineMutabilityHolderHolderStringTest extends EngineMutability2Test[Holder, Holder, String] with HolderHolderStringTest {
  protected def resetMutableParams(holders: (Holder, Holder), s: String) = { holders._1.value = s; holders._2.value = s }
}

@RunWith(classOf[JUnitRunner])
class EngineMutabilityLensSHolderHolderHolderStringTest extends EngineMutability3Test[Holder, Holder, Holder, String] with HolderHolderHolderStringTest {
  protected def resetMutableParams(holders: (Holder, Holder, Holder), s: String) = { holders._1.value = s; holders._2.value = s; holders._3.value = s }

}
