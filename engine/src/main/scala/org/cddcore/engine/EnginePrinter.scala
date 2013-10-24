package org.cddcore.engine

trait IfThenPrinter {
  def start(engine: Engine): String
  def incIndent: String
  def resultPrint(e: Engine, indent: String, conclusion: Conclusion): String
  def ifPrint(e: Engine, indent: String, decision: Decision): String
  def elsePrint(e: Engine, indent: String, decision: Decision): String
  def endPrint(e: Engine, indent: String, decision: Decision): String
  def titlePrint(e: Engine, indent: String, test: Test): String
  def end: String
}

class DefaultIfThenPrinter extends IfThenPrinter {
  def start(e: Engine): String = ""
  val incIndent = " "
  def ifPrint(e: Engine, indent: String, decision: Decision) =
    indent + "if(" + decision.prettyString + ")\n"
  def resultPrint(e: Engine, indent: String, conclusion: Conclusion) =
    indent + conclusion.code.pretty + ":" + conclusion.scenarios.map((s) => s.titleString).mkString(",") + "\n"
  def elsePrint(e: Engine, indent: String, decision: Decision) =
    indent + "else\n";
  def endPrint(e: Engine, indent: String, decision: Decision) = "";
  def titlePrint(e: Engine, indent: String, test: Test) = "Adding "+test+"\n"
  def end: String = ""
}

trait HtmlForIfThenPrinter extends IfThenPrinter {
  import Strings._
  def nameForRequirement: NameForRequirement
  def scenarioPrefix: Option[Any]
  def start(e: Engine): String = s"<html><head><title>Decision Tree for ${e.titleString}</title></head><style>\n${Files.getFromClassPath(getClass, "cddIfThen.css")}</style><body>"
  val incIndent = "  "
  def nbsp(i: String) = "<div class='indent'>" + i.replace(" ", "&nbsp;") + "</div>"

  def ifPrint(e: Engine, indent: String, decision: Decision, becauseClassName: String) =
    s"<div class='if'>${nbsp(indent)}<span class='keyword'>if&nbsp;</span> <div class='$becauseClassName'>(${htmlEscape(decision.prettyString)})</div></div><br />\n"

  def resultPrint(e: Engine, indent: String, conclusion: Conclusion, conclusionClassName: String) = {
    val scenarioHtml = conclusion.scenarios.map((s) => {
      val name =nameForRequirement( s) + ".scenario.html"
      s"<a class='scenario' href='$name' ><img height='15' width='15' src=http://img546.imageshack.us/img546/6186/d6qt.png title='${htmlEscape(s.titleString)}' alt='Test' /></a>"
    }).mkString
    s"<div class='result'>${nbsp(indent)}<span class='keyword'>then&nbsp;</span>$scenarioHtml<div class='$conclusionClassName'>${htmlEscape(conclusion.code.pretty)}</div><div class='tests'></div></div><br />\n"
  }

  def elsePrint(e: Engine, indent: String, decision: Decision) = s"<div class='else'>${nbsp(indent)}<span class='keyword'>else&nbsp;</span></div><br />\n";
  def titlePrint(e: Engine, indent: String, test: Test) = "";
  def endPrint(e: Engine, indent: String, decision: Decision) = "";
  def end = "</body></html>";
}

class HtmlIfThenPrinter(val nameForRequirement: NameForRequirement = new CachedNameForRequirement, val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
  def ifPrint(e: Engine, indent: String, decision: Decision): String = ifPrint(e, indent, decision, "because")
  def resultPrint(e: Engine, indent: String, conclusion: Conclusion): String = resultPrint(e, indent, conclusion, "result")
}

class HtmlWithTestIfThenPrinter(test: Test, val nameForRequirement: NameForRequirement = new CachedNameForRequirement, val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
  def ifPrint(e: Engine, indent: String, decision: Decision): String =
    try {
      if (e.evaluateBecauseForDecision(decision, test.params))
        ifPrint(e, indent, decision, "becauseTrue")
      else
        ifPrint(e, indent, decision, "because")
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        ifPrint(e, indent, decision, "because")
    }
  def resultPrint(e: Engine, indent: String, conclusion: Conclusion): String =
    if (conclusion.scenarios.contains(test))
      resultPrint(e, indent, conclusion, "conclusionWithTest")
    else
      resultPrint(e, indent, conclusion, "conclusion")
}

