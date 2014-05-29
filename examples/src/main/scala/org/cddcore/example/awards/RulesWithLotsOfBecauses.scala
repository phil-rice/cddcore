package org.cddcore.example.awards

import org.junit.runner.RunWith
import org.cddcore.engine.Engine
import org.cddcore.tests.CddJunitRunner
import scala.language.implicitConversions

@RunWith(classOf[CddJunitRunner])
object RulesWithLotsOfBecauses {
case class FourBooleans(a: Boolean, b: Boolean, c: Boolean, d: Boolean)
  implicit def toFourBooleans(x: (Boolean, Boolean, Boolean, Boolean)) = FourBooleans(x._1, x._2, x._3, x._4)
  Engine.logging = true
  val builder = Engine[FourBooleans, String]().
    scenario((false, false, false, false), "-").expected("none").
    scenario((true, false, false, false), "A").expected("a").because((w: FourBooleans) => w.a).
    scenario((true, false, true, false), "AC").expected("d").because((w: FourBooleans) => w.a && w.c).
    scenario((false, true, false, false), "B").expected("b").because((w: FourBooleans) => w.b).
    scenario((true, false, true, true), "ACD").expected("d").because((w: FourBooleans) => w.a && true)

  val engine = Engine.test(builder.build)
//  val engine = builder.build

//  def main(args: Array[String]) {
//    println(engine.asInstanceOf[EngineWithConstructionString].constructionString)
//    println("================== Exceptions ================ ")
//    println(engine.asInstanceOf[EngineWithScenarioExceptionMap].scenarioExceptionMap)
//  }

}