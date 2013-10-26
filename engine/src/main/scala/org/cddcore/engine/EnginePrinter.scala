package org.cddcore.engine

import org.cddcore.engine.tests.CddRunner

trait IfThenPrinter {
  type ReqList = List[Reportable]
  def start(path: ReqList, e: Engine): String
  def ifPrint(path: ReqList, decision: Decision): String
  def resultPrint(path: ReqList, conclusion: Conclusion): String
  def elsePrint(path: ReqList, decision: Decision): String
  def endPrint(path: ReqList, decision: Decision): String
  def end: String

  def indent(path: ReqList): String = "".padTo(path.size, " ").mkString
  def engine(path: ReqList) = path.collect{case (e: Engine) => e}.head
}

class DefaultIfThenPrinter extends IfThenPrinter {
  def start(path: ReqList, e: Engine): String = ""
  def ifPrint(path: ReqList, decision: Decision) =
    indent(path) + "if(" + decision.prettyString + ")\n"
  def resultPrint(path: ReqList, conclusion: Conclusion) =
    indent(path) + conclusion.code.pretty + ":" + conclusion.scenarios.map((s) => s.titleString).mkString(",") + "\n"
  def elsePrint(path: ReqList, decision: Decision) =
    indent(path) + "else\n";
  def endPrint(path: ReqList, decision: Decision) = "";
  def end: String = ""
}

class FullHtmlPage(delegate: IfThenPrinter) extends IfThenPrinter {
  def start(path: ReqList, e: Engine): String = s"<html><head><title>Decision Tree for ${e.titleString}</title></head><style>\n${Files.getFromClassPath(getClass, "cdd.css")}</style><body>"
  def end: String = "</body></html>"

  def ifPrint(path: ReqList, decision: Decision) = delegate.ifPrint(path, decision)
  def resultPrint(path: ReqList, conclusion: Conclusion) = delegate.resultPrint(path, conclusion)
  def elsePrint(path: ReqList, decision: Decision) = delegate.elsePrint(path, decision)
  def endPrint(path: ReqList, decision: Decision) = delegate.endPrint(path, decision)
}

trait HtmlForIfThenPrinter extends IfThenPrinter {
  import Strings._
  def nameForRequirement: NameForRequirement
  def scenarioPrefix: Option[Any]
  def start(path: ReqList, e: Engine): String = ""
  def nbsp(i: String) = "<div class='indent'>" + i.replace(" ", "&nbsp;") + "</div>"

  def highlightedScenarioIcon = "http://img407.imageshack.us/img407/3948/o96r.png"
  def normalScenarioIcon = "http://img201.imageshack.us/img201/1442/a9t.png"

  def scenarioIconLink(s: Test) = normalScenarioIcon

  def scenarioLink(s: Test) = {
    val name = nameForRequirement(s) + ".scenario.html"
    s"<a class='scenarioLink' href='$name' ><img height='15' width='15' src='${scenarioIconLink(s)}' title='${htmlEscape(s.titleString)}' alt='Test' /></a>"
  }

  def ifPrint(path: ReqList, decision: Decision, becauseClassName: String) =
    s"<div class='if'>${nbsp(indent(path))}<span class='keyword'>if&nbsp;</span> <div class='$becauseClassName'>(${htmlEscape(decision.prettyString)})</div><!-- $becauseClassName --></div><!-- if -->\n"

  def resultPrint(path: ReqList, conclusion: Conclusion, conclusionClassName: String) = {
    val scenarioHtml = conclusion.scenarios.map(scenarioLink(_)).mkString
    s"<div class='result'>${nbsp(indent(path))}<span class='keyword'>then&nbsp;</span>$scenarioHtml<div class='$conclusionClassName'>${htmlEscape(conclusion.code.pretty)}</div></div><!-- result -->\n"
  }

  def elsePrint(path: ReqList, decision: Decision) = s"<div class='else'>${nbsp(indent(path))}<span class='keyword'>else&nbsp;</span></div>\n";
  def endPrint(path: ReqList, decision: Decision) = "";
  def end = "";
}

class HtmlIfThenPrinter(val nameForRequirement: NameForRequirement = new CachedNameForRequirement, val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
  def ifPrint(path: ReqList, decision: Decision): String = 
    ifPrint(path, decision, "because")
  def resultPrint(path: ReqList, conclusion: Conclusion): String = 
    resultPrint(path, conclusion, "conclusion")
}

class HtmlWithTestIfThenPrinter(test: Test, val nameForRequirement: NameForRequirement = new CachedNameForRequirement, val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
  def ifPrint(path: ReqList, decision: Decision): String =
    try {
      if (engine(path).evaluateBecauseForDecision(decision, test.params))
        ifPrint(path, decision, "becauseTrue")
      else
        ifPrint(path, decision, "because")
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        ifPrint(path, decision, "because")
    }

  override def scenarioIconLink(s: Test) = if (s == test) highlightedScenarioIcon else normalScenarioIcon
  def resultPrint(path: ReqList, conclusion: Conclusion): String =
    if (conclusion.scenarios.contains(test))
      resultPrint(path, conclusion, "conclusionWithTest")
    else
      resultPrint(path, conclusion, "conclusion")
}

