

package org.cddcore.engine
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StructuredMapTest extends AbstractTest {
  import scala.language.implicitConversions
  val keyStrategy = new SimpleKeyStrategy()
  implicit def split(key: String) = keyStrategy.newKey(key)
  val map = StructuredMap("1.2.2" -> "one.two.two", "1" -> "one", "1.2" -> "one.two", "1.3" -> "one.three", "2.2" -> "two.two", "1.2.11" -> "one.two.eleven")

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

  it should "allow the empty key to be used to get values" in {
    val newMap = map.put("", "Root")
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

  "The default Key strategy" should "split keys based on dot" in {
    assertEquals(List(), split("").path)
    assertEquals(List("1"), split("1").path)
    assertEquals(List("1a"), split("1a").path)
    assertEquals(List("1", "2"), split("1.2").path)
  }

  it should "make new keys" in {
    keyStrategy.newKey("");
    assertEquals(Key("", List()), split(""))
    assertEquals(Key("1", List("1")), split("1"))
    assertEquals(Key("1.2", List("1", "2")), split("1.2"))
  }

  it should "trim all elements in the path" in {
    assertEquals(List("1", "2", "3", "4"), split(" 1 . 2 .3.4").path)
    assertEquals("1.2.3.4", split(" 1 . 2 .3.4").key)
    assertEquals("", split("  ").key)

  }
  it should "make new keys at depth from an old key" in {
    assertEquals(Key("", List()), keyStrategy.newKeyAtDepth("1.2.2", 0))
    assertEquals(Key("1", List("1")), keyStrategy.newKeyAtDepth("1.2.2", 1))
  }

  "The Key Orderer" should "try and use the integer representation of keys if it can, before using the string representation" in {
    implicit def keyToDataAndChildren(key: String) = DataAndChildren(key, None, List())
    val order = new KeyOrder(1)
    assertEquals(-1, order.compare("1.1", "1.2"))
    assertEquals(-9, order.compare("1.2", "1.11"))
    assertEquals(48, order.compare("1.a", "1.11"))
    assertEquals(47, order.compare("1.1a", "1.12"))

  }
}