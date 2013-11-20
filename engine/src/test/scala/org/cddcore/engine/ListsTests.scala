package org.cddcore.engine

import org.junit.runner.RunWith
import Lists.toLists
import org.scalatest.junit.JUnitRunner
@RunWith(classOf[JUnitRunner])
class ListsTests extends AbstractTest {

  "An increasingLists call" should "return List(List(1), List(1,2), List(1,2,3)) where 1,2,3 represent the values in the list" in {
    import Lists._
    assertEquals(List(List(1), List(1, 2), List(1, 2, 3), List(1, 2, 3, 4)), List(1, 2, 3, 4).increasingList)
    assertEquals(List(List(1,2,3,4), List(1, 2,3), List(1, 2), List(1)), List(1, 2, 3, 4).decreasingList)
  }
}