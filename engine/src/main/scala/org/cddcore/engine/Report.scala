package org.cddcore.engine

import org.joda.time.DateTime
import org.cddcore.engine.tests.CddRunner
import java.io.File
import org.scalatest.junit.JUnitRunner

case class Report(reportTitle: String, requirements: Requirement*) extends RequirementHolder {
  val title = Some(reportTitle)
  val children = requirements.toList
  def description = None
  def priority = 0
  def references = List[Reference]()
  def html = foldWithPath(ReqPrintContext())(RequirementsPrinter.html).result
}
trait NameForRequirement {
  def apply(r: Requirement): String

  def apply(stringOrRequirements: Any*): String = stringOrRequirements.map((s) =>
    Strings.clean(s match {
      case r: Requirement => (apply(r))
      case s: String => s
    })).mkString("/")

  def /(strings: Any*): String = "/" + apply(strings: _*)

  def dir(file: File, stringOrRequirements: Any*): File = new File(file, apply(stringOrRequirements: _*))
  def file(file: File, ext: String, stringOrRequirements: Any*): File = new File(file, apply(stringOrRequirements: _*) + "." + ext)
}

class NoNameForRequirement extends NameForRequirement {
   def apply(r: Requirement): String = throw new IllegalStateException
   override def hashCode = 0
   override def equals(other: Any) = other.isInstanceOf[NoNameForRequirement]
}

class CachedNameForRequirement extends NameForRequirement {
  var reqId = 0
  var cache = Map[Requirement, String]()
  def apply(r: Requirement) =
    cache.get(r) match {
      case Some(s) => s;
      case _ => {
        reqId += 1
        val result = Strings.clean(r.titleOrDescription(r.templateName + reqId)).replace(" ", "_");
        cache += (r -> result)
        result
      }
    }

}

class WebPagesCreator(project: Project, dir: File = CddRunner.directory, nameForRequirement: NameForRequirement = new CachedNameForRequirement) {

  def junitHtml(r: Report) = r.foldWithPath(ReqPrintContext(nameForRequirement))(RequirementsPrinter.html).result
  def decisionTreeHtml(e: Engine, ifThenPrinter: IfThenPrinter) = e.toStringWith(ifThenPrinter)

  def createJunitReport(r: Requirement*) = {
    val last = r.last
    val name = "JUnit for " + last.templateName + " " + last.titleString
    val report = Report(name.trim, last)

    Files.printToFile(nameForRequirement.file(dir, "html", (r :+ report): _*))((p) => p.append(junitHtml(report)))
  }

  def createEngineReport(e: Engine) = createJunitReport(e)

  def createDecisionTreeReports =
    for (e: Engine <- project.children) {
      var file = nameForRequirement.file(dir, "html", project, e, "decisionTree")
      Files.printToFile(file)((p) => p.append {
        val report = Report("Decision Tree for " + nameForRequirement(e), e)
        val s = report.foldWithPath(ReqPrintContext(nameForRequirement))(RequirementsPrinter.decisionTree()).result
        s
      })
    }
  def createScenarioReports =
    for (e <- project.children)
      e.collect {
        case test: Test =>
          val file = nameForRequirement.file(dir, "scenario.html", project, e, test)
 
          Files.printToFile(file)((p) => p.append {
            val report = Report("Decision Tree for " + nameForRequirement(e), e)
            val s = report.foldWithPath(ReqPrintContext(nameForRequirement))(RequirementsPrinter.decisionTree(Some(test))).result
            s
          })

      }

  def create {
    createJunitReport(project);
    createDecisionTreeReports
    for (e <- project.children)
      createJunitReport(project, e)
    createScenarioReports
  }

}