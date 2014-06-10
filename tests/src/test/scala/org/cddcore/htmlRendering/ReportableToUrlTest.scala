package org.cddcore.htmlRendering

import scala.language.implicitConversions
import org.cddcore.engine._
import org.cddcore.engine.builder.Decision
import org.cddcore.engine.builder.DecisionTreeNode
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ReportableHelper._
import org.cddcore.utilities.NestedHolder
import org.cddcore.utilities.KeyedMapException
import java.util.NoSuchElementException

@RunWith(classOf[JUnitRunner])
class ReportableToUrlTest extends AbstractTest with SomeHoldersForTest with ReportableTestFramework {

  val emptyR2U = new SimpleReportableToUrl("rootUrl")

  "An empty UrlMap" should "throw exceptions or return none if things aren't found in it" in {
    evaluating { emptyR2U(en1) } should produce[KeyedMapException]
    assertEquals(None, emptyR2U.get(en1))
    evaluating { emptyR2U("") } should produce[NoSuchElementException]
    assertEquals(None, emptyR2U.get(""))
  }

  "A ReportableToUrl" should "return a unique name for a reportable that is at the head of a path with one item in it" in {
    val urlMap = emptyR2U + List(en1)
    val id = urlMap.getName(en1)
    assertEquals("en1", id)
  }

  it should "rreturn a unique name for a reportable that is at the head of a path with multiple items in it" in {
    val path = List(en1, holderEn1)
    val urlMap = emptyR2U + path

    val id = urlMap.getName(en1)
    val idHolder = urlMap.getName(holderEn1)
    assertEquals("en1", id)
    assertEquals("holder1", idHolder)

  }

  it should "return true if the reportable passed to contains has been seen" in {
    val urlMap = emptyR2U + (List(en1, holderEn1))
    assertEquals(true, urlMap.contains(holderEn1))
    assertEquals(true, urlMap.contains(en1))
    assertEquals(false, urlMap.contains(en2))
  }

  it should "produce a unique human readable name for each reportable" in {
    val reqs = List(rep1, rep2, rep3, rep4, rep5, rep1a, rep1b)
    val reportableToUrl = emptyR2U + reqs
    assertEquals("rep1", reportableToUrl.getName(rep1))
    assertEquals("rep1", reportableToUrl.getName(rep1))
    assertEquals("rep2", reportableToUrl.getName(rep2))
    assertEquals("rep2", reportableToUrl.getName(rep2))
    assertEquals("rep3", reportableToUrl.getName(rep3))
    assertEquals("rep3", reportableToUrl.getName(rep3))
    assertEquals("rep4", reportableToUrl.getName(rep4))
    assertEquals("rep4", reportableToUrl.getName(rep4))
    assertEquals("rep5", reportableToUrl.getName(rep5))
    assertEquals("rep5", reportableToUrl.getName(rep5))

    //these are a little awkward to test as the unique calculation might well differ between test runs. So simple equality isn't enough
    val names = reqs.foldLeft(Set[String]())((acc, r) => acc + reportableToUrl.getName(r))
    assertEquals(7, names.size)
  }

  it should "give things with an empty string for a name (after cleaning) an sensible name" in {
    val reportableToUrl = emptyR2U + List(repEmpty)
    val name = reportableToUrl.getName(repEmpty);
    assertEquals(s"Req${repEmpty.textOrder}", name)
  }

  it should "make a urlId from the template name with text order as suffix" in {
    val reportableToUrl = emptyR2U + List(rep1) + List(rep1a)
    assertEquals(s"Req_${rep1.textOrder}", UrlMap.urlId(rep1))
    assertEquals(s"Req_${rep1a.textOrder}", UrlMap.urlId(rep1a))
  }

  it should "make a url from the template name of the head with path" in {
    def checkUrl(expected: String, rs: Reportable*) = {
      val ru = emptyR2U + rs.toList;
      assertEquals(expected, ru(rs.head));
      assertEquals(Some(expected), ru.get(rs.head))
    }
    checkUrl("rootUrl/reqHolder12/rep1.Req.html", rep1, reqHolder12)
    checkUrl("rootUrl/rep1/reqHolder12.ReqAndHolder.html", reqHolder12, rep1)
  }

  it should "make a urlMap from all the reportables in the path" in {
    val ru = emptyR2U ++ reqHolder12.pathsIncludingSelf
    def checkUrl(expected: String, r: Reportable) = {
      assertEquals(expected, ru(r));
      assertEquals(Some(expected), ru.get(r))
    }
    checkUrl("rootUrl/reqHolder12.ReqAndHolder.html", reqHolder12);
    checkUrl("rootUrl/reqHolder12/rep1.Req.html", rep1);
    checkUrl("rootUrl/reqHolder12/rep2.Req.html", rep2);
    assertEquals(3, ru.reportableCount)
  }
  it should "make a urlMap from all the reportables in the container" in {
    val ru = emptyR2U ++ reqHolder12
    def checkUrl(expected: String, r: Reportable) = {
      assertEquals(expected, ru(r));
      assertEquals(Some(expected), ru.get(r))
    }
    checkUrl("rootUrl/reqHolder12.ReqAndHolder.html", reqHolder12);
    checkUrl("rootUrl/reqHolder12/rep1.Req.html", rep1);
    checkUrl("rootUrl/reqHolder12/rep2.Req.html", rep2);
    assertEquals(3, ru.reportableCount)
  }

  //  it should "amalgamate all the referenced documents" in {
  //    import ReportableHelper._
  //    val ru = emptyR2U + reqHolderD12.documentsAndEngines
  //    assert(!ru.contains(doc0))
  //    assert(ru.contains(doc1))
  //    assert(ru.contains(doc2))
  //    assert(ru.contains(reqHolderD12))
  //
  //    assertEquals("rootUrl/reqHolderD12/name1.Document.html", ru(doc1))
  //    assertEquals("rootUrl/reqHolderD12/name2.Document.html", ru(doc2))
  //  }
  //
  //
  //  it should "make a urlMap from reportables and decision / conclusions " in {
  //    val e = Engine[Int, String]().
  //      useCase("").scenario(1).expected("x").
  //      useCase("").
  //      scenario(2).
  //      expected("y").
  //      because((x: Int) => x == 2).
  //      build
  //    val reportableToUrl = new SimpleReportableToUrl
  //
  //    val holder = Holder("EngineHolder", List(e))
  //    val urlMap = reportableToUrl.makeUrlMapWithDecisionsAndConclusions(holder)
  //    val uc1 = e.asRequirement.useCases(0)
  //    val uc2 = e.asRequirement.useCases(1)
  //    val s1 = e.asRequirement.scenarios(0)
  //    val s2 = e.asRequirement.scenarios(1)
  //    val tree = e.asInstanceOf[EngineFromTests[Int, (Int) => Boolean, String, (Int) => String]].tree
  //    import tree._
  //    val d = tree.root.asInstanceOf[Decision[Int, (Int) => Boolean, String, (Int) => String]]
  //    val c1 = d.yes
  //    val c2 = d.no
  //    val ed = e.asRequirement
  //    checkUrl(reportableToUrl, urlMap, holder)
  //    checkUrl(reportableToUrl, urlMap, e, holder)
  //    checkUrl(reportableToUrl, urlMap, d, e, holder)
  //    checkUrl(reportableToUrl, urlMap, c1, d, e, holder)
  //    checkUrl(reportableToUrl, urlMap, c2, d, e, holder)
  //
  //    checkUrl(reportableToUrl, urlMap, ed, holder)
  //    checkUrl(reportableToUrl, urlMap, uc1, ed, holder)
  //    checkUrl(reportableToUrl, urlMap, s1, uc1, ed, holder)
  //    checkUrl(reportableToUrl, urlMap, uc2, ed, holder)
  //    checkUrl(reportableToUrl, urlMap, s2, uc2, ed, holder)
  //    assertEquals(10, urlMap.size)
  //  }
  //
  //  it should "make a urlMap for folding engines" in {
  //    implicit def toFoldingEngine[Params, R, Full](e: Engine[Params, R]) = e.asInstanceOf[FoldingEngine[Params, R, Full]]
  //    val f = Engine.foldList[Int, String].
  //      childEngine("").useCase("uc1").scenario(1).expected("x").scenario(2).expected("y").because((x: Int) => x == 2).
  //      childEngine("").
  //      scenario(1).expected("x").scenario(2).expected("y").because((x: Int) => x == 2).
  //      build
  //    val reportableToUrl = new SimpleReportableToUrl
  //
  //    val holder = Holder("EngineHolder", List(f))
  //    val urlMap = reportableToUrl.makeUrlMapWithDecisionsAndConclusions(holder)
  //    val ce1 = f.engines(0)
  //    val ce2 = f.engines(1)
  //    val uc1 = f.asRequirement.useCases(0)
  //    val s11 = ce1.asRequirement.scenarios(0)
  //    val s12 = ce1.asRequirement.scenarios(1)
  //    val s21 = ce2.asRequirement.scenarios(0)
  //    val s22 = ce2.asRequirement.scenarios(1)
  //    val d1 = ce1.tree.root.asInstanceOf[Decision[Int, (Int) => Boolean, String, (Int) => String]]
  //    val c11 = d1.yes
  //    val c12 = d1.no
  //    val d2 = ce2.tree.root.asInstanceOf[Decision[Int, (Int) => Boolean, String, (Int) => String]]
  //    val c21 = d2.yes
  //    val c22 = d2.no
  //
  //    val fed = f.asRequirement
  //    val ed1 = ce1.asRequirement
  //    val ed2 = ce2.asRequirement
  //    checkUrl(reportableToUrl, urlMap, holder)
  //    checkUrl(reportableToUrl, urlMap, f, holder)
  //    checkUrl(reportableToUrl, urlMap, ce1, f, holder)
  //    checkUrl(reportableToUrl, urlMap, d1, ce1, f, holder)
  //    checkUrl(reportableToUrl, urlMap, c11, d1, ce1, f, holder)
  //    checkUrl(reportableToUrl, urlMap, c12, d1, ce1, f, holder)
  //    checkUrl(reportableToUrl, urlMap, ce2, f, holder)
  //    checkUrl(reportableToUrl, urlMap, d2, ce2, f, holder)
  //    checkUrl(reportableToUrl, urlMap, c21, d2, ce2, f, holder)
  //    checkUrl(reportableToUrl, urlMap, c22, d2, ce2, f, holder)
  //
  //    checkUrl(reportableToUrl, urlMap, fed, holder)
  //    checkUrl(reportableToUrl, urlMap, ed1, fed, holder)
  //    checkUrl(reportableToUrl, urlMap, uc1, ed1, fed, holder)
  //    checkUrl(reportableToUrl, urlMap, s11, uc1, ed1, fed, holder)
  //    checkUrl(reportableToUrl, urlMap, s12, uc1, ed1, fed, holder)
  //    checkUrl(reportableToUrl, urlMap, ed2, fed, holder)
  //    checkUrl(reportableToUrl, urlMap, s21, ed2, fed, holder)
  //    checkUrl(reportableToUrl, urlMap, s22, ed2, fed, holder)
  //    assertEquals(18, urlMap.size)
  //  }

  //  it should "combine the path from the human readable names with the apply method" in {
  //    val reportableToUrl = new SimpleReportableToUrl
  //    assertEquals("rep2/rep1", reportableToUrl(List(rep1, rep2)))
  //    assertEquals("rep2:rep1", reportableToUrl(List(rep1, rep2), separator = ":"))
  //    assertEquals("Req3:rep1", reportableToUrl(List(rep1, rep1b), separator = ":"))
  //  }
  //  def checkUrl(reportableToUrl: ReportableToUrl, urlMap: UrlMap, r: Reportable*) {
  //    assertEquals(reportableToUrl.url(r.toList).get, urlMap(r.head))
  //  }
}