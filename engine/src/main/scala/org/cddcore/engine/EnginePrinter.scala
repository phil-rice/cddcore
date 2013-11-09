package org.cddcore.engine

import org.cddcore.engine.tests.CddRunner

trait IfThenPrinter {
  type ReqList = List[Reportable]
  def start(path: ReqList, e: Engine[_]): String
  def ifPrint(path: ReqList, decision: Decision): String
  def resultPrint(path: ReqList, conclusion: Conclusion): String
  def elsePrint(path: ReqList, decision: Decision): String
  def endPrint(path: ReqList, decision: Decision): String
  def end: String

  def indent(path: ReqList): String = "".padTo(path.size, " ").mkString
  def engine(path: ReqList) = path.collect { case (e: Engine[_]) => e }.head
}

class DefaultIfThenPrinter extends IfThenPrinter {
  def start(path: ReqList, e: Engine[_]): String = ""
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
  def start(path: ReqList, e: Engine[_]): String = s"<html><head><title>Decision Tree for ${e.titleString}</title></head><style>\n${Files.getFromClassPath(getClass, "cdd.css")}</style><body>"
  def end: String = "</body></html>"

  def ifPrint(path: ReqList, decision: Decision) = delegate.ifPrint(path, decision)
  def resultPrint(path: ReqList, conclusion: Conclusion) = delegate.resultPrint(path, conclusion)
  def elsePrint(path: ReqList, decision: Decision) = delegate.elsePrint(path, decision)
  def endPrint(path: ReqList, decision: Decision) = delegate.endPrint(path, decision)
}

object HtmlForIfThenPrinter {
  import Reportable._
  import Strings._
  def nbsp(i: String) = "<div class='indent'>" + i.replace(" ", "&#160;") + "</div>"

  def highlightedScenarioIcon = "http://img407.imageshack.us/img407/3948/o96r.png"
  def normalScenarioIcon = "http://img201.imageshack.us/img201/1442/a9t.png"

  def scenarioLink(urlMap: UrlMap, s: Test, selected: Boolean) = {
    val imgSrc = if (selected) highlightedScenarioIcon else normalScenarioIcon
    val imageHtml = s"<img height='15' width='15' src='$imgSrc' title='${htmlEscape(s.titleString)}' alt='Test' />"
    urlMap.get(s) match {
      case Some(url) => s"<a class='scenarioLink' href='$url' >$imageHtml</a>"
      case _ => imageHtml
    }
  }

}
trait HtmlForIfThenPrinter extends IfThenPrinter {
  import Reportable._
  import HtmlForIfThenPrinter._
  import Strings._

  def reportableToUrl: ReportableToUrl
  def urlMap: UrlMap
  def scenarioPrefix: Option[Any]
  def start(path: ReqList, e: Engine[_]): String = ""
  def ifPrint(path: ReqList, decision: Decision, ifClassName: String) =
    s"<div class='decision'><div class='$ifClassName'>${nbsp(indent(path))}<span class='keyword'>if&#160;</span> <div class='because'>(${htmlEscape(decision.prettyString)})</div><!-- because --></div><!-- $ifClassName -->\n"

  def isSelected(t: Test) = false

  def resultPrint(path: ReqList, conclusion: Conclusion, resultClassName: String) = {
    val scenarioHtml = conclusion.scenarios.map((s) => scenarioLink(urlMap, s, isSelected(s))).mkString
    s"<div class='$resultClassName'>${nbsp(indent(path))}<span class='keyword'>then&#160;</span>$scenarioHtml<div class='conclusion'>${htmlEscape(conclusion.code.pretty)}</div><!-- conclusion --></div><!-- $resultClassName -->\n"
  }

  def elsePrint(path: ReqList, decision: Decision) = s"<div class='else'>${nbsp(indent(path))}<span class='keyword'>else&#160;</span></div>\n";
  def endPrint(path: ReqList, decision: Decision) = "</div><!--decision-->\n";
  def end = "";
}

class HtmlIfThenPrinter(val reportableToUrl: ReportableToUrl = new FileSystemReportableToUrl, val urlMap: UrlMap = UrlMap(Map(), Map()), val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
  def ifPrint(path: ReqList, decision: Decision): String =
    ifPrint(path, decision, "because")

  def resultPrint(path: ReqList, conclusion: Conclusion): String =
    resultPrint(path, conclusion, "result")
}

class HtmlWithTestIfThenPrinter(params: List[Any], optConclusion: Option[Conclusion], test: Option[Test], val reportableToUrl: ReportableToUrl = new FileSystemReportableToUrl, val urlMap: UrlMap, val scenarioPrefix: Option[Any] = None) extends HtmlForIfThenPrinter {
  import HtmlForIfThenPrinter._
  override def isSelected(t: Test) = Some(t) == test
  def ifPrint(path: ReqList, decision: Decision): String =
    try {
      val eval = engine(path).evaluateBecauseForDecision(decision, params)
      val className = (eval, optConclusion) match {
        case (true, Some(c)) if decision.allYesConclusion contains c => "ifTrueOnPath"
        case (true, _) => "ifTrue"
        case (false, Some(c)) if decision.allNoConclusion contains c => "ifFalseOnPath"
        case _ => "if"
      }
      ifPrint(path, decision, className)
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        ifPrint(path, decision, "if")
    }

  def resultPrint(path: ReqList, conclusion: Conclusion): String =
    if (optConclusion == Some(conclusion) || (test.isDefined && conclusion.scenarios.contains(test.get)))
      resultPrint(path, conclusion, "resultWithTest")
    else
      resultPrint(path, conclusion, "result")
}

