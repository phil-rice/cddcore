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
  def html = foldWithPath(ReqPrintContext())(RequirementsPrinter.html(rootUrl)).result
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

  val rootFile: File = nameForRequirement.file(project, junitReport(project))
  val rootUrl = Some("file:///" + rootFile.getPath())
  def junitHtml(r: Report) = r.foldWithPath(ReqPrintContext(nameForRequirement))(RequirementsPrinter.html(rootUrl)).result
  def decisionTreeHtml(e: Engine, ifThenPrinter: IfThenPrinter) = e.toStringWith(ifThenPrinter)

  def junitReport(r: Requirement*) = {
    val last = r.last
    Report("Website", last)
  }

  //  def createJunitReport(r: Requirement*) {
  //    Files.printToFile(rootFile)((p) => p.append(junitHtml(junitReport(r: _*))))
  //  }
  //
  //  def createEngineReport(e: Engine) = createJunitReport(e)
  //
  //  def createDecisionTreeReports =
  //    for (e: Engine <- project.children) {
  //      var file = nameForRequirement.file(project, e)
  //      Files.printToFile(file)((p) => p.append {
  //        val report = Report("Decision Tree for " + nameForRequirement(e), e)
  //        val s = report.foldWithPath(ReqPrintContext(nameForRequirement))(RequirementsPrinter.decisionTree(rootUrl = rootUrl)).result
  //        s
  //      })
  //    }
  //  def createScenarioReports =
  //    for (e <- project.children)
  //      e.collect {
  //        case test: Test =>
  //          val file = nameForRequirement.file(project, e, test)
  //
  //          Files.printToFile(file)((p) => p.append {
  //            val report = Report("Scenario " + nameForRequirement(test), e)
  //            val s = report.foldWithPath(ReqPrintContext(nameForRequirement))(RequirementsPrinter.decisionTree(Some(test), rootUrl)).result
  //            s
  //          })
  //
  //      }

  def print(path: List[Requirement], printer: RequirementsFolderWithPath[ReqPrintContext], root: Requirement) {
    val file = nameForRequirement.file(path.toSeq: _*)
    println(file)
    Files.printToFile(file)((p) => p.append {
      val r = path.last
      val report = Report(r.templateName + " " + r.titleString, root)
      val s = report.foldWithPath(ReqPrintContext(nameForRequirement))(printer).result
      s
    })
  }

  private def engine(path: List[Requirement]) = path(2).asInstanceOf[Engine]

  def create {
    val report = junitReport(project)
    report.walkWithPath((path) => {
      val r = path.last
      val printerAndroot = r match {
        case p: Project => Some(p, RequirementsPrinter.html(rootUrl))
        case e: Engine => Some(e, RequirementsPrinter.html(rootUrl))
        case u: RequirementHolder => Some(u, RequirementsPrinter.html(rootUrl))
        case s: Test => Some(engine(path), RequirementsPrinter.decisionTree(Some(s), rootUrl))
        case _ => None
      }
      printerAndroot match {
        case Some((root, printer)) => print(path, printer, root)
        case _ => ;
      }

    })

  }

}