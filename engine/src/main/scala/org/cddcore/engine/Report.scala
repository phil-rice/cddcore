package org.cddcore.engine

import java.io.File
import org.cddcore.engine.tests.CddRunner
import org.scalatest.junit.JUnitRunner

object Report {
  def apply(reportTitle: String, requirements: Requirement*): Report = Report(reportTitle, None, requirements: _*)
}
case class Report(reportTitle: String, rootUrl: Option[String], requirements: Requirement*) extends RequirementHolder {
  val title = Some(reportTitle)
  val children = requirements.toList
  def description = None
  def priority = 0
  def references = List[Reference]()
  def html(urlMap: Map[Requirement, String] = Map()) = foldWithPath(ReqPrintContext())(RequirementsPrinter.html(rootUrl, urlMap)).result
}

trait NameForRequirement {
  protected var reqId = 0
  protected def requirementToName(r: Requirement) = { reqId += 1; Strings.clean(r.titleOrDescription(r.templateName + reqId)).replace(" ", "_") }

  def dir: File
  def apply(r: Requirement): String

  def apply(stringOrRequirements: Any*): String = stringOrRequirements.map((s) =>
    Strings.clean(s match {
      case r: Requirement => (apply(r))
      case s: String => s
    })).mkString("/")

  def /(strings: Any*): String = "/" + apply(strings: _*)

  def dir(stringOrRequirements: Any*): File =
    new File(dir, apply(stringOrRequirements: _*))
  def file(stringOrRequirements: Any*): File =
    new File(dir, apply(stringOrRequirements: _*) + "." + stringOrRequirements.last.asInstanceOf[Requirement].templateName + ".html")
  def url(stringOrRequirements: Any*): String =
    "file:///" + file(stringOrRequirements: _*).getAbsolutePath()
}

class NoNameForRequirement extends NameForRequirement {
  def dir: File = CddRunner.directory
  def apply(r: Requirement): String = requirementToName(r)
  override def hashCode = 0
  override def equals(other: Any) = other.isInstanceOf[NoNameForRequirement]
}

class CachedNameForRequirement(val dir: File = CddRunner.directory) extends NameForRequirement {

  var cache = Map[Requirement, String]()
  def apply(r: Requirement) =
    cache.get(r) match {
      case Some(s) => s;
      case _ => {

        val result = requirementToName(r);
        cache += (r -> result)
        result
      }
    }

}

class WebPagesCreator(project: Project, nameForRequirement: NameForRequirement = new CachedNameForRequirement) {
  val report = junitReport(project)
  val urlMap = report.foldWithPath(Map[Requirement, String]())(new SimpleFolderWithPath[Map[Requirement, String]]() {
    def fn(reqList: List[Requirement], acc: Map[Requirement, String], r: Requirement): (List[Requirement], Map[Requirement, String]) = {
      val url = nameForRequirement.url((reqList :+ r).toSeq: _*)
      (reqList, acc + (r -> url))
    }
  })
  val rootUrl = Some(nameForRequirement.url(report))
  def junitHtml(r: Report) = r.foldWithPath(ReqPrintContext(nameForRequirement))(RequirementsPrinter.html(rootUrl, urlMap)).result
  def decisionTreeHtml(e: Engine, ifThenPrinter: IfThenPrinter) = e.toStringWith(ifThenPrinter)

  def junitReport(r: Requirement*) = {
    val last = r.last
    Report("Website", last)
  }

  def print(path: List[Requirement], printer: RequirementsFolderWithPath[ReqPrintContext], root: Requirement) {
    val file = nameForRequirement.file(path.toSeq: _*)
    println(file)
    Files.printToFile(file)((p) => p.append {
      val r = path.last
      val report = r match { case report: Report => report; case _ => Report(r.templateName + " " + r.titleString, root) }
      val s = report.foldWithPath(ReqPrintContext(nameForRequirement))(printer).result
      s
    })
  }

  private def engine(path: List[Requirement]) = path(2).asInstanceOf[Engine]

  def create {
    report.walkWithPath((path) => {
      val r = path.last
      if (!urlMap.contains(r))
        throw new IllegalStateException
      val printerAndroot = r match {
        case p: Project => Some((p, RequirementsPrinter.html(rootUrl, urlMap)))
        case e: Engine => Some((e, RequirementsPrinter.html(rootUrl, urlMap)))
        case u: RequirementHolder => Some(u, RequirementsPrinter.html(rootUrl, urlMap))
        case s: Test => Some(engine(path), RequirementsPrinter.decisionTree(Some(s), rootUrl, urlMap))
        case _ => None
      }
      printerAndroot match {
        case Some((root, printer)) => print(path, printer, root)
        case _ => ;
      }

    })

  }

}