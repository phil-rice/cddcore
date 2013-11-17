package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

trait ReportableTestFramework {
  implicit def stringToOption(s: String) = Some(s)
  case class Holder(val name: String, val children: List[Reportable]) extends ReportableHolder
  case class Req(val title: Option[String], description: Option[String], references: List[Reference] = List(), priority: Option[Int] = None) extends Requirement

  val rep1 = Req("rep1", "d1")
  val rep2 = Req("rep2", "d2")
  val rep3 = Req("rep3", "d3")
  val rep4 = Req("rep4", "d4")
  val rep5 = Req("rep5", "d5")

  val holder12 = Holder("holder12", List(rep1, rep2))
  val holder345 = Holder("holder23", List(rep3, rep4, rep5))
  val holder_12_345 = Holder("holder_12_23", List(holder12, holder345))
}

@RunWith(classOf[JUnitRunner])
class ReportableTests extends AbstractTest with ReportableTestFramework {
  import Reportable._

  "A Reportable Holder" should "have a for each method that goes over all descendants recursively" in {
    assertEquals(List(rep2, rep1), holder12.foldLeft(List[Reportable]())((acc, r) => r :: acc))
    assertEquals(List(rep5, rep4, rep3, holder345, rep2, rep1, holder12), holder_12_345.foldLeft(List[Reportable]())((acc, r) => r :: acc))
  }

  it should "walk with paths over itself and all it's descendants" in {
    import Reportable._
    var map = Map[Reportable, ReportableList]()
    holder_12_345.walkWithPath((list) => map += (list.head -> list))
    assertEquals(List(rep1, holder12, holder_12_345), map(rep1))
    assertEquals(List(rep2, holder12, holder_12_345), map(rep2))
    assertEquals(List(holder12, holder_12_345), map(holder12))

    assertEquals(List(rep3, holder345, holder_12_345), map(rep3))
    assertEquals(List(rep4, holder345, holder_12_345), map(rep4))
    assertEquals(List(rep5, holder345, holder_12_345), map(rep5))
    assertEquals(List(holder345, holder_12_345), map(holder345))

    assertEquals(List(holder_12_345), map(holder_12_345))
    assertEquals(8, map.size)
  }

  it should "fold with paths over itself and all it's descendants" in {
    val map = holder_12_345.foldWithPath(List(), Map[Reportable, ReportableList](), (acc: Map[Reportable, ReportableList], list) => acc + (list.head -> list))
    assertEquals(List(rep1, holder12, holder_12_345), map(rep1))
    assertEquals(List(rep2, holder12, holder_12_345), map(rep2))
    assertEquals(List(holder12, holder_12_345), map(holder12))

    assertEquals(List(rep3, holder345, holder_12_345), map(rep3))
    assertEquals(List(rep4, holder345, holder_12_345), map(rep4))
    assertEquals(List(rep5, holder345, holder_12_345), map(rep5))
    assertEquals(List(holder345, holder_12_345), map(holder345))

    assertEquals(List(holder_12_345), map(holder_12_345))
    assertEquals(8, map.size)
  }

  "A FileSystemReportableToUrl" should "Make a nice name for a reportable" in {
    val toUrl = new FileSystemReportableToUrl
    assertEquals("rep1", toUrl(rep1))
    assertEquals("87aklsjd_fj123", toUrl(Req("(*&(87aklsjd_fj123", "d1")))
    assertEquals("d1", toUrl(Req(None, "d1")))
    assertEquals("Req4", toUrl(Req(None, None)))
  }

  it should "Not allow duplicate names" in {
    val toUrl = new FileSystemReportableToUrl
    assertEquals("rep1", toUrl(Req("rep1", Some("1"))))
    assertEquals("Req2", toUrl(Req("rep1", Some("2"))))
    assertEquals("Req3", toUrl(Req("Req2", Some("3"))))
  }

  it should "make a url out of the path fragments" in {
    val toUrl = new FileSystemReportableToUrl
    assertEquals(Some("file:///C:\\Users\\Phil\\.cdd\\rep3\\rep2\\rep1.Req.html"), toUrl.url(List(rep1, rep2, rep3)))

  }

  "A ReportWalker.childWalker" should "fold with paths with startEnd over itself and all it's descendants" in {
    val map = ReportWalker.childWalker.foldWithPath(List(holder_12_345), Map[String, ReportableList](),
      (acc: Map[String, ReportableList], list) => acc + (list.head + "_start" -> list),
      (acc: Map[String, ReportableList], list) => acc + (list.head + "_child" -> list),
      (acc: Map[String, ReportableList], list) => acc + (list.head + "_end" -> list))

    assertEquals(List(rep1, holder12, holder_12_345), map(rep1 + "_child"))
    assertEquals(List(rep2, holder12, holder_12_345), map(rep2 + "_child"))
    assertEquals(List(holder12, holder_12_345), map(holder12 + "_start"))
    assertEquals(List(holder12, holder_12_345), map(holder12 + "_end"))

    assertEquals(List(rep3, holder345, holder_12_345), map(rep3 + "_child"))
    assertEquals(List(rep4, holder345, holder_12_345), map(rep4 + "_child"))
    assertEquals(List(rep5, holder345, holder_12_345), map(rep5 + "_child"))
    assertEquals(List(holder345, holder_12_345), map(holder345 + "_start"))
    assertEquals(List(holder345, holder_12_345), map(holder345 + "_end"))

    assertEquals(List(holder_12_345), map(holder_12_345 + "_start"))
    assertEquals(List(holder_12_345), map(holder_12_345 + "_end"))
    assertEquals(11, map.size)
  }
  "A ReportWalker.engineConclusionWalker" should "fold with paths with startEnd over things up to the engine then just the conclusions" in {
    val e = Engine[Int, String]().useCase("uc1").scenario(0).expected("X").because((x: Int) => x == 0).build

    type Acc = List[(ReportableList, String)]
    val called = ReportWalker.engineConclusionWalker.foldWithPath(List(e), List[(ReportableList, String)](),
      (acc: Acc, list) => acc :+ (list, "start"),
      (acc: Acc, list) => acc :+ (list, "child"),
      (acc: Acc, list) => acc :+ (list, "end"))

    val d1 = e.root.right.get
    val c1 = d1.yes.left.get
    val c2 = d1.no.left.get
    assertEquals((List(e), "start"), called(0))
    assertEquals((List(c1, e), "child"), called(1))
    assertEquals((List(c2, e), "child"), called(2))
    assertEquals((List(e), "end"), called(3))
    assertEquals(4, called.size)

  }

}