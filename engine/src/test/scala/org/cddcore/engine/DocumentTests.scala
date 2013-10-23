package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DocumentTests extends AbstractTest {

  implicit def stringToRef(s: String) = Reference(ref = s)

  def checkCompare(left: Reference, right: Reference, expected: Int) {
    assertEquals(expected, left.compareTo(right))
    assertEquals(-expected, right.compareTo(left))
  }
  "A reference" should "compare based on the ref" in {
    checkCompare("111", "11", 100)
    checkCompare("11", "6", 5)
    checkCompare("1.11.7", "1.6.7", 5)
    checkCompare("1.111.7", "1.11.7", 100)
    checkCompare("", "", 0)
    checkCompare("1", "1", 0)
    checkCompare("2", "1", 1)
    checkCompare("1.6.7", "1.6.7", 0)
    checkCompare("1.ab.7.1", "1.aa.7.1", 1)
    checkCompare("1.11.7.1", "1.11.7", 1)
  }

}