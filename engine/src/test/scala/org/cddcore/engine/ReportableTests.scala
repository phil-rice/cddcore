package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

trait ReportableTestFramework {
  implicit def stringToOption(s: String) = Some(s)
  case class Holder(val name: String, val children: List[Reportable]) extends ReportableHolder {
  }
  case class ReqAndHolder(val name: String, val children: List[Reportable]) extends RequirementAndHolder {
    def title: Option[String] = Some(name)
    def description: Option[String] = Some("desc_" + name)
    def priority: Option[Int] = None
    def references: Set[org.cddcore.engine.Reference] = Set()
  }
  case class Req(val title: Option[String], description: Option[String], references: Set[Reference] = Set(), priority: Option[Int] = None) extends Requirement

  val rep1 = Req("rep1", "d1")
  val rep2 = Req("rep2", "d2")
  val rep3 = Req("rep3", "d3")
  val rep4 = Req("rep4", "d4")
  val rep5 = Req("rep5", "d5")

  val holder12 = Holder("holder12", List(rep1, rep2))
  val holder345 = Holder("holder345", List(rep3, rep4, rep5))
  val holder_12_345 = Holder("holder_12_345", List(holder12, holder345))

  val doc0 = Document(Some("name0"))
  val doc1 = Document(Some("name1"))
  val doc2 = Document(Some("name2"))

  val ref0 = Reference("0", None)
  val ref1 = Reference("1", Some(doc1))
  val ref2 = Reference("2", Some(doc2))

  val rd0 = Req("rd0", "d1", Set(ref0))
  val rd1 = Req("rd1", "d1", Set(ref1))
  val rd2 = Req("rd2", "d2", Set(ref2))
  val rd12 = Req("rd12", "d12", Set(ref1, ref2))
  val holderD12 = Holder("holder12", List(rd1, rd2))

  val reqHolder12 = ReqAndHolder("reqHolder12", List(rep1, rep2))
  val reqHolder345 = ReqAndHolder("reqHolder345", List(rep3, rep4, rep5))
  val reqHolder_12_345 = ReqAndHolder("reqHolder_12_345", List(reqHolder12, reqHolder345))

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
    val map = holder_12_345.foldWithPath(Map[Reportable, ReportableList](), (acc: Map[Reportable, ReportableList], list) => acc + (list.head -> list))
    assertEquals(List(rep1, holder12, holder_12_345), map(rep1))
    assertEquals(List(rep2, holder12, holder_12_345), map(rep2))
    assertEquals(List(holder12, holder_12_345), map(holder12))

    assertEquals(List(rep3, holder345, holder_12_345), map(rep3))
    assertEquals(List(rep4, holder345, holder_12_345), map(rep4))
    assertEquals(List(rep5, holder345, holder_12_345), map(rep5))
    assertEquals(List(holder345, holder_12_345), map(holder345))

    assertEquals(List(holder_12_345), map(holder_12_345))
    assertEquals(8, map.size)
    val order = holder_12_345.foldWithPath(List[Reportable](), (acc: List[Reportable], list) => acc :+ list.head)
    assertEquals(List(holder_12_345, holder12, rep1, rep2, holder345, rep3, rep4, rep5), order)
  }

  it should "fold with paths over itself and all it's descendants with reversed children" in {
    val map = holder_12_345.foldWithPathReversingChildren(Map[Reportable, ReportableList](), (acc: Map[Reportable, ReportableList], list) => acc + (list.head -> list))
    assertEquals(List(rep1, holder12, holder_12_345), map(rep1))
    assertEquals(List(rep2, holder12, holder_12_345), map(rep2))
    assertEquals(List(holder12, holder_12_345), map(holder12))

    assertEquals(List(rep3, holder345, holder_12_345), map(rep3))
    assertEquals(List(rep4, holder345, holder_12_345), map(rep4))
    assertEquals(List(rep5, holder345, holder_12_345), map(rep5))
    assertEquals(List(holder345, holder_12_345), map(holder345))

    assertEquals(List(holder_12_345), map(holder_12_345))
    assertEquals(8, map.size)
    val order = holder_12_345.foldWithPathReversingChildren(List[Reportable](), (acc: List[Reportable], list) => acc :+ list.head)
    assertEquals(List(holder_12_345, holder345, rep5, rep4, rep3, holder12, rep2, rep1), order)
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
    val url = toUrl.url(List(rep1, rep2, rep3)).get
    assert (url.startsWith("file:///"))
    assert (url.endsWith("\\.cdd\\rep3\\rep2\\rep1.Req.html"))
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
    implicit def toEngine[R](e: Engine) = e.asInstanceOf[EngineBuiltFromTests[R]]
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

  "A DocumentAndEngineWalker when passed a project" should "call project_start, project_end then visit the documents / engines as child functions" in {
    val d1 = Document(title = Some("aa"))
    val d2 = Document(title = Some("zz"))
    val e1 = Engine[Int, String]().title("e1").useCase("uc0").scenario(0).expected("x").reference("", d2).build
    val e2 = Engine[Int, String]().title("e2").useCase("uc1").scenario(1).expected("y").reference("", d1).build
    val p = Project("SomeProject", e1, e2)
    type Acc = List[(ReportableList, String)]
    val called = ReportWalker.documentThenEngineWalker.foldWithPath(List(p), List[(ReportableList, String)](),
      (acc: Acc, list) => acc :+ (list, "start"),
      (acc: Acc, list) => acc :+ (list, "child"),
      (acc: Acc, list) => acc :+ (list, "end"))
    val docHolder = DocumentHolder(List(d1, d2))
    val eHolder = EngineHolder(List(e1,e2))
    assertEquals((List(p), "start"), called(0))
    assertEquals((List(docHolder, p), "start"), called(1))
    assertEquals((List(d1, docHolder, p), "child"), called(2))
    assertEquals((List(d2, docHolder, p), "child"), called(3))
    assertEquals((List(docHolder, p), "end"), called(4))
    assertEquals((List(eHolder, p), "start"), called(5))
    assertEquals((List(e1, eHolder, p), "child"), called(6))
    assertEquals((List(e2, eHolder, p), "child"), called(7))
    assertEquals((List(eHolder, p), "end"), called(8))
    assertEquals((List(p), "end"), called(9))
    assertEquals(10, called.size)

  }

}