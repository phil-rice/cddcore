package org.cddcore.example.awards

import org.junit.runner.RunWith
import org.cddcore.engine.Engine
import org.cddcore.engine.tests.CddJunitRunner
import scala.language.implicitConversions
import org.cddcore.engine.EngineBuiltFromTests
import org.cddcore.engine.EngineWithConstructionString

case class Widget(a: Boolean, b: Boolean, c: Boolean, d: Boolean)
@RunWith(classOf[CddJunitRunner])
object RulesWithLotsOfBecauses {
  implicit def toWidget(x: (Boolean, Boolean, Boolean, Boolean)) = Widget(x._1, x._2, x._3, x._4)
  Engine.logging = true
  val builder = Engine[Widget, String]().
    scenario((false, false, false, false), "FFFF").expected("none").
    scenario((true, false, false, false), "TFFF").expected("a").because((w: Widget) => w.a).
    scenario((true, false, true, false), "TFTF").expected("d").because((w: Widget) => w.a && w.c).
    scenario((false, true, false, false), "FTFF").expected("b").because((w: Widget) => w.b).
    scenario((true, false, true, true), "TFTT").expected("d").because((w: Widget) => w.a && true)

  val engine = Engine.test(builder.build)

  def main(args: Array[String]) {
    println(engine.asInstanceOf[EngineWithConstructionString].constructionString)
  }

}