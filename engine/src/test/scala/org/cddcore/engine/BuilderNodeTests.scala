package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine._

@RunWith(classOf[JUnitRunner])
class BuilderNodeTests extends EngineStringStringTests {
  val c: Code = (s: String) => "x"

  "A builder" should "have its builder nodes setable" in {
    val b1 = builder.description("a description").code(c).expected("x").priority(2);
    val b2 = builder.description("a description").code(c).expectException(new RuntimeException).priority(2);
    assertEquals(Some("a description"), b1.description);
    assertEquals(Some(c), b1.optCode);
    assertEquals(Some(ROrException[String]("x")), b1.expected);
    assertEquals(Some(ROrException[String](new RuntimeException)), b2.expected);
    assertEquals(2, b1.priority);
  }

  it should "throw exceptions if values are set twice" in {
    val b1 = builder.description("a description").code(c).expected("x").priority(2);
    val b2 = builder.description("a description").code(c).expectException(new RuntimeException).priority(2);

    evaluating { b1.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b1.code(c) } should produce[CannotDefineCodeTwiceException]
    evaluating { b1.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b1.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
  }

  "A use case" should "have its builder nodes setable" in {
    val b1 = builder.useCase("one").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").code(c).expectException(new RuntimeException).priority(2);
    val uc1 = b1.useCases(0);
    val uc2 = b2.useCases(0);
    assertEquals(Some("one"), uc1.description);
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

  it should "throw exceptions if values are set twice" in {
    val b1 = builder.useCase("one").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").code(c).expectException(new RuntimeException).priority(2);

    evaluating { b1.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b1.code(c) } should produce[CannotDefineCodeTwiceException]
    evaluating { b1.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b1.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
  }

  "A scenario" should "have its builder nodes setable" in {
    val b1 = builder.useCase("one").scenario("x").description("one").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").scenario("x", "two").code(c).expectException(new RuntimeException).priority(2);
    val s1 = b1.useCases(0).scenarios(0);
    val s2 = b2.useCases(0).scenarios(0);
    assertEquals(Some("one"), s1.description);
    assertEquals(Some("two"), s2.description);
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

  it should "throw exceptions if values are set twice" in {
    val b1 = builder.useCase("one").scenario("x").description("one").code(c).expected("x").priority(2);
    val b2 = builder.useCase("two").scenario("x", "two").code(c).expectException(new RuntimeException).priority(2);

    evaluating { b1.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b2.description("again") } should produce[CannotDefineDescriptionTwiceException]
    evaluating { b1.code(c) } should produce[CannotDefineCodeTwiceException]
    evaluating { b1.expected("again") } should produce[CannotDefineExpectedTwiceException]
    evaluating { b1.expectException(new RuntimeException) } should produce[CannotDefineExpectedTwiceException]
  }

}