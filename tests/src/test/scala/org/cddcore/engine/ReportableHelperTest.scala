package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReportableTestHelper extends AbstractTest {

  val doc1 = Document(Some("Doc1"))
  val doc2 = Document(Some("Doc2"))
  val doc3 = Document(Some("Doc3"))
  val e = Engine[Int, Int].reference("1", doc1).useCase("").reference("2", doc2).scenario(0).expected(0).reference("3", doc3).build

  "The reportable test helper" should "return all the contained documents" in {
    import ReportableHelper._
    assertEquals(Set(doc1, doc2, doc3), e.asRequirement.documents.toSet)
  }

}