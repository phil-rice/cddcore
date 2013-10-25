package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import RequirementsPrinter._
@RunWith(classOf[JUnitRunner])
class EngineRequirementTests extends EngineStringStringTests {
  def checkBuilderPrinter(holder: RequirementHolder, expected: String) {
    import Renderer._
    val printer = RequirementsPrinter("+B $indent$/$title$/$description$\n",
      "+UC $indent$/$title$/$description$\n",
      "S $indent$/$title$/$description$\n",
      "-UC $indent$/$title$/$description$\n",
      "-B $indent$/$title$/$description$\n")
    val result = holder.foldWithPath( ReqPrintContext())(printer)
    assertEquals(ReqPrintContext(1, expected, new NoNameForRequirement), result)

  }

  "A builder" should "print out its self, usecases and scenarios" in {
    checkBuilderPrinter(
      Engine[String, String]().title("bt").description("bd"),
      "+B 1/bt/bd\r\n" +
        "-B 1/bt/bd\r\n")
    checkBuilderPrinter(
      Engine[String, String]().title("bt").description("bd").
        useCase("UC1").description("uc_d_1").
        scenario("x", "title1").description("s_d_1"),
      "+B 1/bt/bd\r\n" +
        "+UC 2/UC1/uc_d_1\r\n" +
        "S 3/title1/s_d_1\r\n" +
        "-UC 2/UC1/uc_d_1\r\n" +
        "-B 1/bt/bd\r\n")

  }
  def checkHtmlPrinter(holder: RequirementHolder, expected: String) {
    val printer = RequirementsPrinter.html(None)
    val result = holder.foldWithPath( ReqPrintContext())(printer)
    assertEquals(ReqPrintContext(1, expected, new NoNameForRequirement), result)
  }

  def paramHtml(p: String)="<tr><td rowspan='1'>Parameter</td><td>"+p+"</td></tr>"
  
  //Commented out as still playing with what it should look like
//  "An html printer" should "Print out html for a builder, it's usecases and scenarios" in {
//    def blankScenarioTable(p: String) = "<table class='scenarioTable'><tr><td>Expected</td><td></td></tr><tr><td>Code</td><td></td></tr>"+ paramHtml(p)+"</table>"
//    checkHtmlPrinter(
//      Engine[String, String]().title("bt").description("bd"),
//      "<div class='builder'><h1>bt</h1><p>bd</p>\r\n</div>\r\n")
//    checkHtmlPrinter(
//      Engine[String, String]().title("bt").description("bd").
//        useCase("UC1").description("uc_d_1").
//        scenario("x", "title1").description("s_d_1"),
//      "<div class='builder'><h1>bt</h1><p>bd</p>\r\n" +
//        "<div class='usecase'><h2>UC1</h2><p>uc_d_1</p>\r\n" +
//        "<div class='scenario'><h3>title1</h3><p>s_d_1</p>" + blankScenarioTable ("x")+ "</div>\r\n" +
//        "</div>\r\n" +
//        "</div>\r\n")
//    checkHtmlPrinter(
//      Engine[String, String]().title("bt").description("bd").
//        useCase("UC1").description("uc_d_1").
//        scenario("x", "title1").description("s_d_1").
//        useCase("UC2").description("uc_d_2").
//        scenario("y", "title2").description("s_d_2").
//        scenario("z", "title3").description("s_d_3"),
//      "<div class='builder'><h1>bt</h1><p>bd</p>\r\n" +
//        "<div class='usecase'><h2>UC2</h2><p>uc_d_2</p>\r\n" +
//        "<div class='scenario'><h3>title3</h3><p>s_d_3</p>" + blankScenarioTable("z") + "</div>\r\n" +
//        "<div class='scenario'><h3>title2</h3><p>s_d_2</p>" + blankScenarioTable("y") + "</div>\r\n" +
//        "</div>\r\n" +
//        "<div class='usecase'><h2>UC1</h2><p>uc_d_1</p>\r\n" +
//        "<div class='scenario'><h3>title1</h3><p>s_d_1</p>" + blankScenarioTable("x") + "</div>\r\n" +
//        "</div>\r\n" +
//        "</div>\r\n")
//  }
//
//  it should "print out the code  if it exists in a table" in {
//    checkHtmlPrinter(
//      Engine[String, String]().title("bt").
//        useCase("UC1").
//        scenario("x", "title1").description("d").code(new CodeFn[B, RFn, String]((x) => x, "code")),
//      "<div class='builder'><h1>bt</h1>\r\n" +
//        "<div class='usecase'><h2>UC1</h2>\r\n" +
//        "<div class='scenario'><h3>title1</h3><p>d</p>" +
//        "<table class='scenarioTable'>" +
//        "<tr><td>Expected</td><td></td></tr>" +
//        "<tr><td>Code</td><td>code</td></tr>" + //<---------
//        paramHtml("x") +
//        "</table>" +
//        "</div>\r\n" +
//        "</div>\r\n" +
//        "</div>\r\n")
//  }
//
//  it should "print out the expected if it exists in a table" in {
//    checkHtmlPrinter(
//      Engine[String, String]().title("bt").
//        useCase("UC1").
//        scenario("x", "title1").description("d").expected("exp"),
//      "<div class='builder'><h1>bt</h1>\r\n" +
//        "<div class='usecase'><h2>UC1</h2>\r\n" +
//        "<div class='scenario'><h3>title1</h3><p>d</p>" +
//        "<table class='scenarioTable'>" +
//        "<tr><td>Expected</td><td>exp</td></tr>" +
//        "<tr><td>Code</td><td></td></tr>" + //<---------
//        paramHtml("x") +
//        "</table>" +
//        "</div>\r\n" +
//        "</div>\r\n" +
//        "</div>\r\n")
//  }
}