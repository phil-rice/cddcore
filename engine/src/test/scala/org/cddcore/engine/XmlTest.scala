package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.xml.Elem
import org.cddcore.structure._

class TestXmlSituation(val xml: Elem) extends XmlSituation {

}
trait XmlTestMother {
  import Xml._

  val x =
    <root>
      <one>1</one>
      <two>2</two>
      <repeated>1</repeated>
      <repeated>2</repeated>
      <repeated>3</repeated>
    </root>
  class xmlSituationZeroFragment(val x: Elem) extends XmlSituation() {
  }
  class xmlSituationOneFragment(val x: Elem) extends XmlSituation() {
    val one = xml(x) \ "one" \ string
  }
  class xmlSituationWithoutType(val x: Elem) extends XmlSituation() {
    val one = xml(x) \ "one"
  }

  class xmlSituationOneFragmentNotFound(x: Elem) extends XmlSituation() {
    val notIn = xml(x) \ "absent" \ string
  }

  class xmlSituationWithoutVariable extends XmlSituation() {
    val notIn = xml(x) \ "absent" \ string
  }

  class xmlSituationThreeFragment(val x: Elem) extends XmlSituation() {
    val one = xml(x) \ "one" \ integer
    val two = xml(x) \ "two" \ string
    val repeatedString = xml(x) \ "repeated" \ string
    val repeatedInteger = xml(x) \ "repeated" \ integer
    val repeatedFolded = xml(x) \ "repeated" \ integer \ Fold(0, (l: Int, r: Int) => l + r)
  }

  class xmlSituationRepeatingFragments extends XmlSituation {
    val x = <root>
              <repeated><nested>1</nested><nested>2</nested></repeated>
              <repeated><nested>3</nested><nested>4</nested></repeated>
              <repeated></repeated>
            </root>
    val repeatedString = xml(x) \ "repeated" \ string
    val repeatedNestedString = xml(x) \ "repeated" \ string
    val repeatedInteger = xml(x) \ "repeated" \ integer
    val repeatedFold = xml(x) \ "repeated" \ integer \ Fold(0, (l: Int, r: Int) => l + r)
    val repeatedNestedFold = xml(x) \ "repeated" \ "nested" \ integer \ Fold(0, (l: Int, r: Int) => l + r)
    val repeatedNestedList = xml(x) \ "repeated" \ "nested" \ integer \ list[Int]
  }
}

@RunWith(classOf[JUnitRunner])
class XmlTest extends AbstractTest with XmlTestMother {

  "An Xml Situation with zero fragments" should "produce a decent toString" in {
    assertEquals("xmlSituationZeroFragment(\n  \n" + //don't really want the two spaces
      ")", new xmlSituationZeroFragment(x).toString)
  }
  "An Xml Situation with one fragments" should "produce a decent toString" in {
    assertEquals("xmlSituationOneFragment(\n" +
      "  one = 1\n" +
      "  Xml: x\n  <root>       <one>1</one>       <two>2</two>       <repeated>1</repeated>       <repeated>2</repeated>       <repeated>3</repeated>     </root>\n" +
      "\\one = 1)", new xmlSituationOneFragment(x).toString)
  }

  "An Xml Situation" should "report No Convertor when not configured properly" in {
    assertEquals("xmlSituationWithoutType(\n" +
      "  one = No Convertor\n" +
      "  Xml: x\n  <root>       <one>1</one>       <two>2</two>       <repeated>1</repeated>       <repeated>2</repeated>       <repeated>3</repeated>     </root>\n" +
      "\\one = No Convertor)", 
      new xmlSituationWithoutType(x).toString)
  }

  "An Xml Situation" should "handle the Elem not being a variable" in {
    assertEquals("xmlSituationWithoutVariable(\n" +
      "  notIn = \n" +
      "  Xml:   <root>       <one>1</one>       <two>2</two>       <repeated>1</repeated>       <repeated>2</repeated>       <repeated>3</repeated>     </root>\n" +
      "\\absent)", new xmlSituationWithoutVariable().toString)
  }

  "An Xml Situation with simple repeating blocks and a fold" should "produce a decent toString" in {
    assertEquals("xmlSituationThreeFragment(\n" +
      "  one = 1\n" +
      "  repeatedFolded = 6\n" +
      "  repeatedInteger = 123\n" +
      "  repeatedString = 123\n" +
      "  two = 2\n" +
      "  Xml: x\n  <root>       <one>1</one>       <two>2</two>       <repeated>1</repeated>       <repeated>2</repeated>       <repeated>3</repeated>     </root>\n" +
      "\\one = 1\n" +
      "\\two = 2\n" +
      "\\repeated = 123,123,6)", new xmlSituationThreeFragment(x).toString)
  }

  "An Xml Situation with nested blocks" should "evaluate fragments" in {
    val situation = new xmlSituationRepeatingFragments
    assertEquals("1234", situation.repeatedString())
    assertEquals("1234", situation.repeatedNestedString())
    assertEquals(1234, situation.repeatedInteger())
    assertEquals(10, situation.repeatedNestedFold())
    assertEquals(List(4, 3, 2, 1), situation.repeatedNestedList())
    evaluating { situation.repeatedFold() } should produce[NumberFormatException]
  }

  "An Xml Situation with nested blocks" should "produce a decent toString" in {
    val situation = new xmlSituationRepeatingFragments
    assertEquals("xmlSituationRepeatingFragments(\n" +
      "  repeatedFold = NumberFormatException For input string: \"\"\n" +
      "  repeatedInteger = 1234\n" +
      "  repeatedNestedFold = 10\n" +
      "  repeatedNestedList = List(4, 3, 2, 1)\n" +
      "  repeatedNestedString = 1234\n" +
      "  repeatedString = 1234\n" +
      "  Xml: x\n  <root>               <repeated><nested>1</nested><nested>2</nested></repeated>               <repeated><nested>3</nested><nested>4</nested></repeated>               <repeated></repeated>             </root>\n" +
      "\\repeated = 1234,1234,1234,NumberFormatException\n" +
      "..\\nested = 10,List(4, 3, 2, 1)\n" +
      "..\\nested = 10,List(4, 3, 2, 1))", situation.toString)
  }

  "An Xml Situation with a fragment that isn't present" should "produce a decent toString" in {
    val situation = new xmlSituationOneFragmentNotFound(x)
    assertEquals("xmlSituationOneFragmentNotFound(\n" +
      "  notIn = \n" +
      "  Xml:   <root>       <one>1</one>       <two>2</two>       <repeated>1</repeated>       <repeated>2</repeated>       <repeated>3</repeated>     </root>\n" +
      "\\absent)", situation.toString)
  }
}

