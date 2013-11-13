package org.cddcore.engine


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MapsTests extends AbstractTest {

  "the walkSelfAndChildrenPaths" should "return an  iterable of paths including the root" in {
    val map = Map[String, List[String]]("a" -> List("b", "c"), "b" -> List("d", "e"))
    assertEquals(List(List("a"), List("a", "b"), List("a", "b", "d"), List("a", "b", "e"), List("a", "c")), Maps.walkSelfAndChildrenPaths(map)("a"))
  }
}