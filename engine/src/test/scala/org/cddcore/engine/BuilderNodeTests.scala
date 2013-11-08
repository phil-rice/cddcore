package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine._

@RunWith(classOf[JUnitRunner])
class BuilderNodeTests extends EngineStringStringTests {
  val c: Code = (s: String) => "x"

  val doc1 = Document(name = Some("x"))

  "A builder" should "have its builder nodes setable" in {
    val b1 = builder.document(doc1).title("title").description("a description").code(c).expected("x").priority(2);
    val b2 = builder.description("a description").code(c).expectException(new RuntimeException).priority(2);
    assertEquals(Some("title"), b1.title);
    assertEquals(Some("a description"), b1.description);
    assertEquals(Some(c), b1.optCode);
    assertEquals(Some(ROrException[String]("x")), b1.expected);
    assertEquals(Some(ROrException[String](new RuntimeException)), b2.expected);
    assertEquals(2, b1.priority);
  }

  it should "throw exceptions if values are set twice" in {
    val b1 = builder.title("title").description("a description").code(c).expected("x").priority(2);
    val b2 = builder.title("title").description("a description").code(c).expectException(new RuntimeException).priority(2);

    evaluating { b1.title("again") } should produce[CannotDefineTitleTwiceException]
    evaluating { b1.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b1.code(c) } should produce[CannotDefineCodeTwiceException]
    evaluating { b1.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b1.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
    evaluating { b2.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b2.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
  }

  it should "allow references to be added" in {
    val b1 = builder.document(doc1).reference("1.1")
    val b2 = builder.document(doc1).reference("1.2", "x").reference("1.3")

    assertEquals(List(Reference("1.1")), b1.references);
    assertEquals(List(Reference("1.3"), Reference("1.2", Some(doc1))), b2.references);
  }
  it should "allow references to be added even after other stuff added" in {
    val b1 = builder.document(doc1).reference("1.1").useCase("")

    assertEquals(List(Reference("1.1")), b1.references);
  }

  "A use case" should "have its builder nodes setable" in {
    val b1 = builder.useCase("one").description("d1").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").description("d2").code(c).expectException(new RuntimeException).priority(2);
    val uc1 = b1.useCases(0);
    val uc2 = b2.useCases(0);
    assertEquals(Some("one"), uc1.title);
    assertEquals(Some("d1"), uc1.description);
    assertEquals(Some(c), uc1.optCode);
    assertEquals(Some(ROrException[String]("x")), uc1.expected);
    assertEquals(Some(ROrException[String](new RuntimeException)), uc2.expected);
    assertEquals(2, uc1.priority);

    assertEquals(None, b1.description);
    assertEquals(None, b1.optCode);
    assertEquals(None, b1.expected);
    assertEquals(None, b2.expected);
    assertEquals(0, b1.priority);
  }

  it should "allow usecase title and description to be set in one go" in {

    val b1 = builder.useCase("one", "d1").code(c).expected("x").priority(2);
    val uc1 = b1.useCases(0);
    assertEquals(Some("one"), uc1.title);
    assertEquals(Some("d1"), uc1.description);
  }

  it should "throw exceptions if values are set twice" in {
    val b1 = builder.useCase("one").description("d").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").description("d").code(c).expectException(new RuntimeException).priority(2);

    evaluating { b1.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b1.code(c) } should produce[CannotDefineCodeTwiceException]
    evaluating { b1.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b1.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
    evaluating { b2.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b2.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
  }

  it should "allow references to be added" in {
    val b1 = builder.document(doc1).useCase("one").code(c).expected("x").priority(2).reference("1.1")
    val b2 = builder.document(doc1).useCase("two").code(c).expectException(new RuntimeException).priority(2).reference("1.2", "x").reference("1.3");
    val uc1 = b1.useCases(0);
    val uc2 = b2.useCases(0);

    assertEquals(List(Reference("1.1")), uc1.references);
    assertEquals(List(Reference("1.3"), Reference("1.2", Some(doc1))), uc2.references);
  }

  "A scenario" should "have its builder nodes setable" in {
    val b1 = builder.useCase("one").scenario("x").title("one").description("d").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").scenario("x", "two").code(c).expectException(new RuntimeException).priority(2);
    val s1 = b1.useCases(0).scenarios(0);
    val s2 = b2.useCases(0).scenarios(0);
    assertEquals(Some("one"), s1.title);
    assertEquals(Some("two"), s2.title);
    assertEquals(Some("d"), s1.description);
    assertEquals(Some(c), s1.optCode);
    assertEquals(Some(ROrException[String]("x")), s1.expected);
    assertEquals(Some(ROrException[String](new RuntimeException)), s2.expected);
    assertEquals(2, s1.priority);

    assertEquals(None, b1.description);
    assertEquals(None, b1.optCode);
    assertEquals(None, b1.expected);
    assertEquals(None, b2.expected);
    assertEquals(0, b1.priority);
  }
  
  it should "allow usecase title and description to be set in one go" in {

    val b1 = builder.useCase("one", "d1").scenario("x", "sc_title", "sc_description")
    val s1 = b1.useCases(0).scenarios(0);
    assertEquals(Some("sc_title"), s1.title);
    assertEquals(Some("sc_description"), s1.description);
  }

  it should "throw exceptions if values are set twice" in {
    val b1 = builder.useCase("one").scenario("x").title("one").description("d").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").scenario("x", "two").code(c).expectException(new RuntimeException).priority(2);

    evaluating { b1.title("again") } should produce[CannotDefineTitleTwiceException]
    evaluating { b2.title("again") } should produce[CannotDefineTitleTwiceException]
    evaluating { b1.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b1.code(c) } should produce[CannotDefineCodeTwiceException]
    evaluating { b1.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b1.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
  }

  it should "allow references to be added" in {
    val b1 = builder.document(doc1).useCase("one").
      scenario("x").description("one").code(c).expected("x").priority(2).reference("1.1")
    val b2 = builder.document(doc1).useCase("two").
      scenario("x", "two").code(c).expectException(new RuntimeException).priority(2).
      reference("1.2", "x").reference("1.3");
    val s1 = b1.useCases(0).scenarios(0);
    val s2 = b2.useCases(0).scenarios(0);

    assertEquals(List(Reference("1.1")), s1.references);
    assertEquals(List(Reference("1.3"), Reference("1.2", Some(doc1))), s2.references);
  }

  "An engine" should "inherit the properties of the builder" in {
    val e = builder.title("title").description("a description").code(c).expected("x").priority(2).build;
    assertEquals(Some("title"), e.title);
    assertEquals(Some("a description"), e.description);
    assertEquals(2, e.priority);
  }

}