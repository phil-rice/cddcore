package org.cddcore.engine

trait IfThenPrinter {
  def start(titleString: String): String
  def incIndent: String
  def resultPrint: (String, Conclusion) => String
  def ifPrint: (String, Decision) => String
  def elsePrint: (String, Decision) => String
  def endPrint: (String, Decision) => String
  def titlePrint: (String, Test) => String
  def end: String
}
class DefaultIfThenPrinter extends IfThenPrinter {
  def start(titleString: String): String = ""
  val incIndent = " "
  def ifPrint = (indent, node) => indent + "if(" + node.prettyString + ")\n"
  def resultPrint = (indent, result) => indent + result.code.pretty + ":" + result.scenarios.map((s) => s.titleString).mkString(",") + "\n"
  def elsePrint = (indent, node) => indent + "else\n";
  def endPrint = (indent, node) => "";
  def titlePrint = (indent, scenario) => "";
  def end: String = ""
}

class HtmlIfThenPrinter extends IfThenPrinter {
  import Strings._
  def start(titleString: String): String = s"<html><head><title>Decision Tree for ${titleString}</title></head><style>\n${Files.getFromClassPath(getClass, "cddIfThen.css")}</style><body>"
  val incIndent = "  "
  def nbsp(i: String) = "<div class='indent'>"+i.replace(" ", "&nbsp;") +"</div>"
  
  def ifPrint = (indent, node) =>
    s"<div class='if'>${nbsp(indent)}<span class='keyword'>if</span> <div class='because'>(${htmlEscape(node.prettyString)})</div></div><br />\n"
  def resultPrint = (indent, result) =>
    s"<div class='result'>${nbsp(indent)}<span class='keyword'>then</span><div class='conclusion'>${htmlEscape(result.code.pretty)}</div><div class='tests'>" +
      result.scenarios.map((s) => s"<a class='scenario' href='' ><img height='15' width='15' src=http://img546.imageshack.us/img546/6186/d6qt.png title='${htmlEscape(s.titleString)}' alt='Test' /></a>").mkString(",") + "</div></div><br />\n"
  def elsePrint = (indent, node) => s"<div class='else'>${nbsp(indent)}<span class='keyword'>else</span></div><br />\n";
  def titlePrint = (indent, scenario) => "";
  def endPrint = (indent, node) => "";
  def end = "</body></html>";
}
object EnginePrinter {

  def apply(e: Engine) = {

  }

}
