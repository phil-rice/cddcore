package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.io.Source

case class RequirementForTest(name: String) extends Requirement {
  def title: Option[String] = Some("Title_" + name)
  def description: Option[String] = Some("desc_" + name)
  def priority: Int = 0
  def references: List[Reference] = List()
  def params: List[Any] = List()
  override def toString() = name
}

class RequirementForTestNoDesc(name: String) extends RequirementForTest(name) {
  override def description = None
}

class RequirementHolderForTest(name: String, val children: List[Requirement]) extends RequirementForTest(name) with RequirementHolder {
  override def toString() = name
}
class RequirementHolderForTestNoDesc(name: String, children: List[Requirement]) extends RequirementHolderForTest(name, children) {
  override def description = None
}

@RunWith(classOf[JUnitRunner])
class RequirementTests extends AbstractTest {

  val r1 = new RequirementForTest("1")
  val r2 = new RequirementForTestNoDesc("2")
  val r3 = new RequirementForTest("3")

  val holder1 = new RequirementHolderForTest("holder1", List())
  val holder2_123 = new RequirementHolderForTest("holder2_123", List(r1, r2, r3))
  val holder3_12 = new RequirementHolderForTest("holder3_12", List(r1, r2))
  val holder3_2_123_and_3_12 = new RequirementHolderForTest("holder3_2_123_and_3_12", List(holder2_123, holder3_12))
  val holder4_1 = new RequirementHolderForTest("holder4_1", List(r1))
  val holder5_2 = new RequirementHolderForTest("holder5_2", List(r2))

  "A requirement holder" should "traverse all of it's children depth first" in {
    assertEquals(List(), holder1.map((r) => r))
    assertEquals(List(r1, r2, r3), holder2_123.map((r) => r))
    assertEquals(List(holder2_123, r1, r2, r3, holder3_12, r1, r2), holder3_2_123_and_3_12.map((r) => r))
  }

  it should "walk all it's children" in {
    def check(holder: RequirementHolder, expected: String*) = {
      var visited = List[String]()
      holder.walk((
        (r: Requirement) => visited = ("+" + r) :: visited,
        (r: Requirement) => visited = ("." + r) :: visited,
        (r: Requirement) => visited = ("-" + r) :: visited))
      assertEquals(expected.toList, visited.reverse)
    }
    check(holder1, "+holder1", "-holder1")
    check(holder2_123, "+holder2_123", ".1", ".2", ".3", "-holder2_123")
    check(holder3_2_123_and_3_12, "+holder3_2_123_and_3_12", "+holder2_123", ".1", ".2", ".3", "-holder2_123", "+holder3_12", ".1", ".2", "-holder3_12", "-holder3_2_123_and_3_12")
  }

  it should "fold all it's children" in {
    def check(holder: RequirementHolder, expected: String*) = {
      val visited = holder.fold(List[String]())(
        SimpleRequirementsFolder[List[String]](
          (acc, r) => ("+" + r) :: acc,
          (acc, r) => ("." + r) :: acc,
          (acc, r) => ("-" + r) :: acc))
      assertEquals(expected.toList, visited.reverse)
    }
    check(holder1, "+holder1", "-holder1")
    check(holder2_123, "+holder2_123", ".1", ".2", ".3", "-holder2_123")
    check(holder3_2_123_and_3_12, "+holder3_2_123_and_3_12", "+holder2_123", ".1", ".2", ".3", "-holder2_123", "+holder3_12", ".1", ".2", "-holder3_12", "-holder3_2_123_and_3_12")
  }

  def checkPrinter(holder: RequirementHolder, expected: String) = assertEquals(ReqPrintContext(1, expected,new NoNameForRequirement), holder.fold(ReqPrintContext())(new SimpleRequirementsPrinter))

  "A Simple Requirement Printer" should "Have a title and description only for an empty list" in {
    checkPrinter(new RequirementHolderForTest("holder", List()), "<h1>Title_holder</h1>\n<p>desc_holder</p>\n")
    checkPrinter(new RequirementHolderForTestNoDesc("holder", List()), "<h1>Title_holder</h1>\n")
  }

  it should "nest title and description for nested requirements" in {
    checkPrinter(holder4_1, "<h1>Title_holder4_1</h1>\n<p>desc_holder4_1</p>\n<h2>Title_1</h2>\n<p>desc_1</p>\n")
  }
  def checkStPrinter(holder: RequirementHolder, expected: String) = {
    val classPathPattern = getClass.getPackage().getName().replace('.', '/') + "/RequirementTestTemplate"
    import RequirementsPrinter._
    val printer = new StRequirementsPrinter(Map(), Map(
      "RequirementHolderForTest_start" -> "Start $indent$/$title$/$description$\n",
      "RequirementForTest" -> "Requirement $indent$/$title$/$description$\n",
      "RequirementForTestNoDesc" -> "RequirementNoDesc $indent$/$title$/$description$\n",
      "RequirementHolderForTest_end" -> "End $indent$/$title$/$description$\n"))
    val result = holder.foldWithPath(ReqPrintContext())(printer)
    assertEquals(ReqPrintContext(1, expected, new NoNameForRequirement), result)
  }

  "A StPrinter" should "print holders and requirements using the relevant template" in {
    checkStPrinter(holder4_1,
      "Start 1/Title_holder4_1/desc_holder4_1\r\n" +
        "Requirement 2/Title_1/desc_1\r\n" +
        "End 1/Title_holder4_1/desc_holder4_1\r\n")

    checkStPrinter(holder2_123,
      "Start 1/Title_holder2_123/desc_holder2_123\r\n" +
        "Requirement 2/Title_1/desc_1\r\n" +
        "RequirementNoDesc 2/Title_2/\r\n" +
        "Requirement 2/Title_3/desc_3\r\n" +
        "End 1/Title_holder2_123/desc_holder2_123\r\n")
  }

}