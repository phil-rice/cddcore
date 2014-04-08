package org.cddcore.utilities

import org.junit.runner.RunWith
import Key.defaultKeyStrategy
import Key.defaultStringToKey
import org.cddcore.engine._
import scala.Option.option2Iterable
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StructuredMapTests extends AbstractTest {
  import Key._
  val map = StructuredMap("1.2.2" -> "one.two.two", "1" -> "one", "1.2" -> "one.two", "1.3" -> "one.three", "2.2" -> "two.two", "1.2.11" -> "one.two.eleven")
  val mapOfLists = StructuredMapOfList("1.2.2" -> "one.two.two", "1" -> "onea", "1" -> "oneb", "1.2" -> "one.two", "1.3" -> "one.three", "2.2" -> "two.two", "1.2.11" -> "one.two.eleven")

  "A structured map" should "allow values to be added and retreived" in {
    assertEquals(None, map.get(""))
    assertEquals(Some("one"), map.get("1"))
    assertEquals(None, map.get("1.1"))
    assertEquals(Some("one.two"), map.get("1.2"))
    assertEquals(Some("one.three"), map.get("1.3"))
    assertEquals(Some("one.two.two"), map.get("1.2.2"))
    assertEquals(Some("two.two"), map.get("2.2"))
    assertEquals(Some("one.two.eleven"), map.get("1.2.11"))
  }

  it should "return the keys of the children" in {
    def checkChildren(key: Key, expected: Key*) = assertEquals(expected.toList, map.kidsOf(key))
    checkChildren("", "1", "2")
    checkChildren("1", "1.2", "1.3")
    checkChildren("1.1")
  }

  it should "allow the empty key to be used to get values" in {
    val newMap = map + ("" -> "Root")
    assertEquals(Some("Root"), newMap.get(""))
    assertEquals(Some("one"), newMap.get("1"))
    assertEquals(None, newMap.get("1.1"))
    assertEquals(Some("one.two"), newMap.get("1.2"))
  }

  it should "walk values in the ordering of the keys specified by the key deconstructor" in {
    var acc = List[(String, Option[String])]()
    map.walk((key, v) => acc = (key -> v) :: acc)
    assertEquals(List(
      "" -> None,
      "1" -> Some("one"),
      "1.2" -> Some("one.two"),
      "1.2.2" -> Some("one.two.two"),
      "1.2.11" -> Some("one.two.eleven"),
      "1.3" -> Some("one.three"),
      "2" -> None,
      "2.2" -> Some("two.two")), acc.reverse)
  }

  it should "fold values in the ordering of the keys specified by the key deconstructor" in {
    val actual = map.fold(List[(String, Option[String])]())((acc, k, ov) => (k -> ov) :: acc)
    assertEquals(List(
      "" -> None,
      "1" -> Some("one"),
      "1.2" -> Some("one.two"),
      "1.2.2" -> Some("one.two.two"),
      "1.2.11" -> Some("one.two.eleven"),
      "1.3" -> Some("one.three"),
      "2" -> None,
      "2.2" -> Some("two.two")), actual.reverse)
  }

  it should "throw DuplicateKeyException if the same key is added" in {
    evaluating { map + ("1" -> "any") } should produce[DuplicateKeyException]
    val newMap = map + ("" -> "Root")
    evaluating { newMap + ("" -> "Root") } should produce[DuplicateKeyException]
  }

  "A structured map of list" should "allow values to be added and retreived" in {
    assertEquals(List(), mapOfLists.get(""))
    assertEquals(List("onea", "oneb"), mapOfLists.get("1"))
    assertEquals(List(), mapOfLists.get("1.1"))
    assertEquals(List("one.two"), mapOfLists.get("1.2"))
    assertEquals(List("one.three"), mapOfLists.get("1.3"))
    assertEquals(List("one.two.two"), mapOfLists.get("1.2.2"))
    assertEquals(List("two.two"), mapOfLists.get("2.2"))
    assertEquals(List("one.two.eleven"), mapOfLists.get("1.2.11"))
  }

  it should "allow things to be added and retreived no matter the order" in {
    val mapOfLists = StructuredMapOfList(
      "1" -> "onea", "1" -> "oneb", "1.2" -> "one.two", "1.2.2" -> "one.two.two", "1.3" -> "one.three", "2.2" -> "two.two", "1.2.11" -> "one.two.eleven")
    assertEquals(List(), mapOfLists.get(""))
    assertEquals(List("onea", "oneb"), mapOfLists.get("1"))
    assertEquals(List(), mapOfLists.get("1.1"))
    assertEquals(List("one.two"), mapOfLists.get("1.2"))
    assertEquals(List("one.three"), mapOfLists.get("1.3"))
    assertEquals(List("one.two.two"), mapOfLists.get("1.2.2"))
    assertEquals(List("two.two"), mapOfLists.get("2.2"))
    assertEquals(List("one.two.eleven"), mapOfLists.get("1.2.11"))

  }

  "The default Key strategy" should "split keys based on dot" in {
    assertEquals(List(), "".path)
    assertEquals(List("1"), "1".path)
    assertEquals(List("1a"), "1a".path)
    assertEquals(List("1", "2"), "1.2".path)
  }

  it should "make new keys" in {
    defaultKeyStrategy.newKey("");
    assertEquals(Key("", List()), "": Key)
    assertEquals(Key("1", List("1")), "1": Key)
    assertEquals(Key("1.2", List("1", "2")), "1.2": Key)
  }

  it should "trim all elements in the path" in {
    assertEquals(List("1", "2", "3", "4"), " 1 . 2 .3.4".path)
    assertEquals("1.2.3.4", " 1 . 2 .3.4".key)
    assertEquals("", "  ".key)

  }
  it should "make new keys at depth from an old key" in {
    assertEquals(Key("", List()), defaultKeyStrategy.newKeyAtDepth("1.2.2", 0))
    assertEquals(Key("1", List("1")), defaultKeyStrategy.newKeyAtDepth("1.2.2", 1))
  }

  "The Key Orderer" should "try and use the integer representation of keys if it can, before using the string representation" in {
    import scala.language.implicitConversions
    implicit def keyToDataAndChildren(key: String) =
      new DataAndChildren[String, Option[String]](key, None, List())
    val order = new KeyOrder[String, Option[String]](1)
    assertEquals(-1, order.compare("1.1", "1.2"))
    assertEquals(-9, order.compare("1.2", "1.11"))
    assertEquals(48, order.compare("1.a", "1.11"))
    assertEquals(47, order.compare("1.1a", "1.12"))

  }
}