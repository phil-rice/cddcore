package org.cddcore.eclipse

import org.junit.runner.RunWith
import org.autotdd.engine._
import org.autotdd.engine.tests.AutoTddRunner

//@RunWith(classOf[AutoTddRunner])
case class EngineDescription(val name: String, val description: String)

class AutoTddParser

@RunWith(classOf[AutoTddRunner])
object AutoTddParser {
  val itemParse = Engine[String, EngineDescription]().
    useCase("").scenario("abc\ndef").expected(EngineDescription("abc", "def")).
    code(
      (item: String) => {
        val index = item.indexOf("\n")
        index match {
          case -1 => throw new IllegalArgumentException(item);
          case _ => EngineDescription(item.substring(0, index), item.substring(index + 1))
        }
      }).build;

  //  itemParse.scenario("abc\ndef", EngineDescription("abc", "def"))
  //  //TODO Need to be able to assert throws exceptions
  //
  //  val parse = Engine1((t: String) => t.split(AutoTddRunner.separator).map(itemParse))
  //  parse.scenario("abc\ndef" + AutoTddRunner.separator + "123\n234",
  //    Array(EngineDescription("abc", "def"), EngineDescription("123", "234")))

  def apply(s: String) = itemParse(s)
}