package org.cddcore.utilities

import org.junit.runner.RunWith
import org.cddcore.engine.AbstractTest
import org.scalatest.junit.JUnitRunner
import StartChildEndType._
@RunWith(classOf[JUnitRunner])
class TraceTest extends AbstractTest {

  type TB = TraceBuilder[String, String, String, String]
  type TI = TraceItem[String, String, String, String]

//  "A StartChildEndTraversable's foreach" should "return itself and child, if no children" in {
//    case class SCE(name: String, children: SCE*) extends StartChildEndTraversable[SCE] {
//      override def toString = s"SCE($name)"
//    }
//    val sce = SCE("empty")
//    val sce0 = SCE("zero")
//    val sce1 = SCE("one")
//    val sceHolds01 = SCE("sceHolds01", sce0, sce1)
//
//    assertEquals(List((sce, Child)), sce.toList)
//    assertEquals(List((sceHolds01, Start), (sce0, Child), (sce1, Child), (sceHolds01, End)), sceHolds01.toList)
//  }
  "A TraceBuilder" should "start with an empty trace" in {
    val tb: TB = TraceBuilder()
    assertEquals(List(), tb.children)
  }
  it should "return a list of trace items " in {
    val tb: TB = TraceBuilder()
    val result = tb.nest("one", "params1").
      finished("result1", Some("evidence1")).
      nest("two", "params2").
      finished("result2", Some("evidence2"))
    assertEquals(List(TraceItem("one", "params1", Right("result1"), Some("evidence1"), List(), 0),
      TraceItem("two", "params2", Right("result2"), Some("evidence2"), List(), 0)), result.children)
  }

  it should "return deal with nested trace items " in {
    val tb: TB = TraceBuilder()
    val result = tb.nest("one", "params1").
      nest("onea", "params1a").finished("result1a", Some("evidence1a")).
      nest("oneb", "params1b").finished("result1b", Some("evidence1b")).
      finished("result1", Some("evidence1")).
      nest("two", "params2").
      finished("result2", Some("evidence2"))
    assertEquals(List(
      TraceItem("one", "params1", Right("result1"), Some("evidence1"), List(
        TraceItem("onea", "params1a", Right("result1a"), Some("evidence1a"), List(), 0),
        TraceItem("oneb", "params1b", Right("result1b"), Some("evidence1b"), List(), 0)), 0),
      TraceItem("two", "params2", Right("result2"), Some("evidence2"), List(), 0)), result.children)
  }
  it should "ignore mains in the ignore list, if the main is the first thing" in {
    val tb: TB = TraceBuilder(List("one"))
    val result = tb.nest("one", "params1").
      nest("onea", "params1a").finished("result1a", Some("evidence1a")).
      nest("oneb", "params1b").finished("result1b", Some("evidence1b")).
      finished("result1", Some("evidence1")).
      nest("two", "params2").
      finished("result2", Some("evidence2"))
    assertEquals(List(TraceItem("two", "params2", Right("result2"), Some("evidence2"), List(), 0)), result.children)
  }
  it should "ignore mains in the ignore list, if the main is in a nested call the first thing" in {
    val tb: TB = TraceBuilder(List("onea"))
    val result = tb.nest("one", "params1").
      nest("onea", "params1a").finished("result1a", Some("evidence1a")).
      nest("oneb", "params1b").finished("result1b", Some("evidence1b")).
      finished("result1", Some("evidence1")).
      nest("two", "params2").
      finished("result2", Some("evidence2"))
    assertEquals(List(
      TraceItem("one", "params1", Right("result1"), Some("evidence1"), List(
        TraceItem("oneb", "params1b", Right("result1b"), Some("evidence1b"), List(), 0)), 0),
      TraceItem("two", "params2", Right("result2"), Some("evidence2"), List(), 0)), result.children)
  }

}