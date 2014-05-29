package org.cddcore.utilities

import org.cddcore.engine.AbstractTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListsTest extends AbstractTest {
  "The increasingList method" should "return a gradually increasing list of the items in the original list" in {
    assertEquals(List(), Lists.increasingList(List()))
    assertEquals(List(List("a")), Lists.increasingList(List("a")))
    assertEquals(List(List("a"), List("a", "b")), Lists.increasingList(List("a", "b")))
    assertEquals(List(List("a"), List("a", "b"), List("a", "b", "c")), Lists.increasingList(List("a", "b", "c")))
  }

  "The decreasingList method" should "return a gradually decreasing list of the items in the original list" in {
    assertEquals(List(List("a")), Lists.decreasingList(List("a")))
    assertEquals(List(List("a", "b"), List("b")), Lists.decreasingList(List("a", "b")))
    assertEquals(List(List("a", "b", "c"), List("b", "c"), List("c")), Lists.decreasingList(List("a", "b", "c")))
    assertEquals(List(), Lists.decreasingList(List()))
  }

  "The suffixesSameCount method" should "return the number of items that are identical at the end of the list " in {
    assertEquals(0, Lists.suffixSameCount(List(), List()))
    assertEquals(0, Lists.suffixSameCount(List(1), List()))
    assertEquals(0, Lists.suffixSameCount(List(1), List()))
    assertEquals(1, Lists.suffixSameCount(List(1), List(1)))
    assertEquals(2, Lists.suffixSameCount(List(1, 2, 3), List(7, 2, 3)))
    assertEquals(2, Lists.suffixSameCount(List(1, 2, 3), List(4, 5, 6, 7, 2, 3)))
    assertEquals(2, Lists.suffixSameCount(List(7, 2, 3), List(1, 2, 3)))
    assertEquals(2, Lists.suffixSameCount(List(4, 5, 6, 7, 2, 3), List(1, 2, 3)))
  }

  "The toStartChildEnd method" should "turn a list of lists into a list of lists with a start / child or end marker" in {
    import StartChildEndType._
    def l[T](ts: T*) = ts.toList
    assertEquals(l(
      (l(1), Child),
      (l(2), Child),
      (l(3), Child)),
      Lists.pathToStartChildEnd(l(
        l(1),
        l(2),
        l(3))))
    assertEquals(l(
      (l(1), Start),
      (l(2, 1), Start),
      (l(3, 2, 1), Child),
      (l(2, 1), End),
      (l(1), End)),
      Lists.pathToStartChildEnd(List(List(1), List(2, 1), List(3, 2, 1))))
    assertEquals(l(
      (l(1), Start),
      (l(2, 1), Start),
      (l(3, 2, 1), Child),
      (l(4, 2, 1), Start),
      (l(5, 4, 2, 1), Child),
      (l(6, 4, 2, 1), Child),
      (l(4, 2, 1), End),
      (l(2, 1), End),
      (l(3, 1), Start),
      (l(7, 3, 1), Child),
      (l(3, 1), End),
      (l(1), End)),

      Lists.pathToStartChildEnd(List(
        List(1),
        List(2, 1),
        List(3, 2, 1),
        List(4, 2, 1),
        List(5, 4, 2, 1),
        List(6, 4, 2, 1),
        List(3, 1),
        List(7, 3, 1))))

    assertEquals(l(
      (l(1), Start),
      (l(2, 1), Start),
      (l(3, 2, 1), Start),
      (l(4, 3, 2, 1), Child),
      (l(3, 2, 1), End),
      (l(2, 1), End),
      (l(5, 1), Child),
      (l(1), End)),
      Lists.pathToStartChildEnd(List(
        List(1),
        List(2, 1),
        List(3, 2, 1),
        List(4, 3, 2, 1),
        List(5, 1))))
  }
}