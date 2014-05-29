package org.cddcore.engine

import scala.language.implicitConversions
import org.cddcore.utilities.CodeHolder
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.htmlRendering.SampleContexts

class RequirementTest

trait SomeHoldersForTest {
  implicit def toSome[X](x: X) = Some(x)

  type EN = BuilderNodeForTest[String, (String) => Boolean, String, (String) => String]
  type ENH = BuilderNodeAndHolderForTest[String, (String) => Boolean, String, (String) => String]
  val en1: EN = BuilderNodeForTest(title = "en1")
  val en2: EN = BuilderNodeForTest(title = "en2")
  val holderEn1: ENH = BuilderNodeAndHolderForTest(title = "holder1", nodes = List(en1))
  val holderEn12: ENH = BuilderNodeAndHolderForTest(title = "holder12", nodes = List(en1, en2))
  val holderHolderEn12: ENH = BuilderNodeAndHolderForTest(title = "holderHolder12", nodes = List(holderEn12))
}

@RunWith(classOf[JUnitRunner])
class EngineHolderTest extends AbstractTest with SomeHoldersForTest {

  "An builder node  holder foreach  method" should "return all the engine nodes" in {
    assertEquals(List(en1), holderEn1.toList)
    assertEquals(List(en1, en2), holderEn12.toList)
    assertEquals(List(holderEn12, en1, en2), holderHolderEn12.toList)
  }
  "An builder node  holder all method" should "return all the engine nodes of the requested class" in {
    assertEquals(List(en1), holderEn1.all(classOf[EN]))
    assertEquals(List(en1), holderEn1.all(classOf[BuilderNode[String, (String) => Boolean, String, (String) => String]]))
    assertEquals(List(en1, en2), holderEn12.all(classOf[EN]))
    val actual = holderHolderEn12.all(classOf[BuilderNode[String, (String) => Boolean, String, (String) => String]])
    assertEquals(List(holderEn12, en1, en2), actual)
    assertEquals(List(en1, en2), holderHolderEn12.all(classOf[EN]))
  }

  "An builder node  holders paths method" should "return all the nodes in a path" in {
    assertEquals(List(
      List(en1)), holderEn1.paths.toList)
    assertEquals(List(
      List(holderEn12),
      List(en1, holderEn12),
      List(en2, holderEn12)),
      holderHolderEn12.paths.toList)
  }


}

