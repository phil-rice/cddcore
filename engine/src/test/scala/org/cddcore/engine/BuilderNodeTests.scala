package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine._

@RunWith(classOf[JUnitRunner])
class BuilderNodeTests extends EngineStringStringTests {
  val c: Code = (s: String) => "x"
  val b: Because[B] = (s: String) => true

  val doc1 = Document(name = Some("x"))

  "A builder" should "have its builder noades setable" in {
    val b1 = builder.document(doc1).title("title").description("a description").code(c).expected("x").priority(2);
    val b2 = builder.description("a description").code(c).expectException(new RuntimeException).priority(2);
    assertEquals(Some("title"), b1.builderData.title);
    assertEquals(Some("a description"), b1.builderData.description);
    assertEquals(Some(c), b1.builderData.optCode);
    assertEquals(Some(ROrException[String]("x")), b1.builderData.expected);
    assertEquals(Some(ROrException[String](new RuntimeException)), b2.builderData.expected);
    assertEquals(Some(2), b1.builderData.priority);
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

    assertEquals(List(Reference("1.1")), b1.builderData.references);
    assertEquals(List(Reference("1.3"), Reference("1.2", Some(doc1))), b2.builderData.references);
  }
  it should "allow references to be added even after other stuff added" in {
    val b1 = builder.document(doc1).reference("1.1").useCase("")

    assertEquals(List(Reference("1.1")), b1.builderData.references);
  }

  it should "allow scenarios without usecases" in {
    val b = builder.scenario("one");
    val b1 = b.description("d1").code(c).expected("x").priority(2);
    val s = firstScenario(b1)
    assertEquals(Some("d1"), s.description)
    assertEquals(Some(c), s.optCode)
    assertEquals(Some(2), s.priority)

  }

  "A use case" should "have its builder nodes setable" in {
    val b1 = builder.useCase("one").description("d1").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").description("d2").code(c).expectException(new RuntimeException).priority(2);
    val uc1 = b1.builderData.children(0).asInstanceOf[UseCase];
    val uc2 = b2.builderData.children(0).asInstanceOf[UseCase];
    assertEquals(Some("one"), uc1.title);
    assertEquals(Some("d1"), uc1.description);
    assertEquals(Some(c), uc1.optCode);
    assertEquals(Some(ROrException[String]("x")), uc1.expected);
    assertEquals(Some(ROrException[String](new RuntimeException)), uc2.expected);
    assertEquals(Some(2), uc1.priority);

    assertEquals(None, b1.builderData.description);
    assertEquals(None, b1.builderData.optCode);
    assertEquals(None, b1.builderData.expected);
    assertEquals(None, b2.builderData.expected);
    assertEquals(None, b1.builderData.priority);
  }

  it should "allow usecase title and description to be set in one go" in {

    val b1 = builder.useCase("one", "d1").code(c).expected("x").priority(2);
    val uc1 = b1.builderData.all(classOf[UseCase])(0);
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
    val uc1 = b1.builderData.all(classOf[UseCase])(0);
    val uc2 = b2.builderData.all(classOf[UseCase])(0);

    assertEquals(List(Reference("1.1")), uc1.references);
    assertEquals(List(Reference("1.3"), Reference("1.2", Some(doc1))), uc2.references);
  }

  "A scenario" should "have its builder nodes setable" in {
    val b1 = builder.useCase("one").scenario("x").title("one").description("d").because(b).code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").scenario("x", "two").because(b).code(c).expectException(new RuntimeException).priority(2);
    val s1 = firstScenario(b1.builderData);
    val s2 = firstScenario(b2.builderData);
    assertEquals(Some("one"), s1.title);
    assertEquals(Some("two"), s2.title);
    assertEquals(Some("d"), s1.description);
    assertEquals(Some(c), s1.optCode);
    assertEquals(Some(b), s1.because);
    assertEquals(Some(ROrException[String]("x")), s1.expected);
    assertEquals(Some(ROrException[String](new RuntimeException)), s2.expected);
    assertEquals(Some(2), s1.priority);

    assertEquals(None, b1.builderData.description);
    assertEquals(None, b1.builderData.optCode);
    assertEquals(None, b1.builderData.expected);
    assertEquals(None, b2.builderData.expected);
    assertEquals(None, b1.builderData.priority);
  }

  it should "allow usecase title and description to be set in one go" in {

    val b1 = builder.useCase("one", "d1").scenario("x", "sc_title", "sc_description")
    val s1 = firstScenario(b1.builderData);
    assertEquals(Some("sc_title"), s1.title);
    assertEquals(Some("sc_description"), s1.description);
  }

  it should "throw exceptions if values are set twice" in {
    val b1 = builder.useCase("one").scenario("x").title("one").description("d").because(b).code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").scenario("x", "two").because(b).code(c).expectException(new RuntimeException).priority(2);

    evaluating { b1.title("again") } should produce[CannotDefineTitleTwiceException]
    evaluating { b2.title("again") } should produce[CannotDefineTitleTwiceException]
    evaluating { b1.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b1.code(c) } should produce[CannotDefineCodeTwiceException]
    evaluating { b1.because(b) } should produce[CannotDefineBecauseTwiceException]
    evaluating { b1.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b1.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
  }

  it should "allow references to be added" in {
    val b1 = builder.document(doc1).useCase("one").
      scenario("x").description("one").code(c).expected("x").priority(2).reference("1.1")
    val b2 = builder.document(doc1).useCase("two").
      scenario("x", "two").code(c).expectException(new RuntimeException).priority(2).
      reference("1.2", "x").reference("1.3");
    val s1 = firstScenario(b1.builderData)
    val s2 = firstScenario(b2.builderData);

    assertEquals(List(Reference("1.1")), s1.references);
    assertEquals(List(Reference("1.3"), Reference("1.2", Some(doc1))), s2.references);
  }

  "An engine" should "inherit the properties of the builder" in {
    val e = builder.title("title").description("a description").code(c).expected("x").priority(2).build;
    assertEquals(Some("title"), e.title);
    assertEquals(Some("a description"), e.description);
    assertEquals(Some(2), e.priority);
  }

  it should "Allow parameter details to be specified " in {
    implicit def toEngineFromTests[R](e: Engine) = e.asInstanceOf[EngineFromTestsImpl]
    val parser1 = (x: String) => x.toInt
    val parser2 = (x: String) => x.toInt + 1
    val e = org.cddcore.engine.Engine[Int, Int, Int].param(parser1, "One").param(parser2, "Two").build
    assertEquals(List(ParamDetail("One", parser1), ParamDetail("Two", parser2)), e.paramDetails)
  }
}