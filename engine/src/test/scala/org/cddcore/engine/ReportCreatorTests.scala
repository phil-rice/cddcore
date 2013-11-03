package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.xml.XML
import scala.xml.Node

@RunWith(classOf[JUnitRunner])
class ReportCreatorTests extends AbstractTest {
  def attributeEquals(name: String, value: String)(node: Node) = {
    val n = node;
    val attribute = node.attribute(name)
    val filtered = attribute.filter(
      (v) =>
        v.toString == value)
    filtered.isDefined
  }

  val eTitle = Engine[Int, String]().title("EngTitle").build
  val pForTitle = Project("ProjName", eTitle);
  "A ReportCreator" should "allow Html for a project to be produced" in {
    val rc = new ReportCreator(pForTitle, "ReportTitle", new SimpleReportableToUrl)
    val html = rc.htmlFor(List(pForTitle)).get
    val xml = XML.loadString(html)
    val rootDivs = xml \ "body" \ "div"
    assertEquals(1, rootDivs.size)
    val rootDivHead = rootDivs.head
    val reportDiv = rootDivHead filter attributeEquals("class", "report")
    val topLine = reportDiv filter attributeEquals("class", "TopLine")
    val cddLogo = topLine filter attributeEquals("class", " cddLogo")
    println("Report: ")
    println(reportDiv)
    println()

    val toplineDiv = rootDivHead filter { (n) => println(n); println; true }
    assertEquals("asd", (xml \\ "div") filter attributeEquals("class", "engine") text)
  }
}