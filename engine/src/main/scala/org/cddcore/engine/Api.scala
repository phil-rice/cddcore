package org.cddcore.engine

object Report {
  def apply(reportTitle: String, requirements: Requirement*): Report = Report(reportTitle, None, requirements: _*)
}

case class Report(reportTitle: String, rootUrl: Option[String], reportables: Reportable*) extends ReportableHolder {
  val title = Some(reportTitle)
  val children = reportables.toList
  val description = None
  val priority = 0
  val references = List[Reference]()
}

case class Project(projectTitle: String, engines: Engine*) extends RequirementAndHolder {
  lazy val documents = engines.flatMap(_.documents).distinct
  lazy val refToRequirement: Map[Reference, Requirement] =
    engines.foldLeft(List[(Reference, Requirement)]())((acc, e) => acc ++
      e.collect { case r: Requirement => r }.flatMap(_.references.map((_, e))) ++
      e.references.map((_, e))).toMap

  val title = Some(projectTitle)
  val children = engines.toList
  def description = None
  def priority = 0
  def references = List()
}

object Engine {
  def apply[P, R]() = new BuilderFactory1[P, R]().builder;
  def apply[P1, P2, R]() = new BuilderFactory2[P1, P2, R]().builder;
  def apply[P1, P2, P3, R]() = new BuilderFactory3[P1, P2, P3, R]().builder;

  def state[S, P, R]() = new BuilderFactory2[S, P, (S, R)]().builder;
  def state[S, P1, P2, R]() = new BuilderFactory3[S, P1, P2, (S, R)]().builder;

}

trait Engine extends Requirement with RequirementAndHolder {
  import Reportable._
  def documents: List[Document]
  def decisionTreeNodes: Int
  def root: Either[Conclusion, Decision]
  def toStringWith(printer: IfThenPrinter): String = toStringWith(List(this), root, printer)
  protected def toStringWith(path: ReportableList, root: Either[Conclusion, Decision], printer: IfThenPrinter): String
  def evaluateBecauseForDecision(decision: Decision, params: List[Any]): Boolean
}

trait Decision extends Reportable {
  def because: List[CodeHolder]
  def yes: Either[Conclusion, Decision]
  def no: Either[Conclusion, Decision]
  def prettyString: String
}

trait Conclusion extends Reportable {
  def code: CodeHolder
  def scenarios: List[Test]
}

case class Document(name: Option[String] = None, title: Option[String] = None, description: Option[String] = None, url: Option[String] = None) {
  def titleString = name.getOrElse(title.getOrElse(url.getOrElse(description.getOrElse(""))))
}

case class RefTree(ref: Reference, children: List[Reference])

case class Reference(ref: String = "", document: Option[Document] = None) extends Comparable[Reference] {

  def titleString = document.collect { case d => d.titleString + " " }.getOrElse("") + ref
  def compareTo(other: Reference): Int = {
    val left = ref.split("\\.")
    val right = other.ref.split("\\.")
    val zipped = left.zipAll(right, "0", "0")
    zipped.map((f) => {
      val (l, r) = f
      try {
        val lInt = l.toInt
        val rInt = r.toInt
        lInt - rInt
      } catch {
        case e: Throwable => {
          l.compareTo(r)
        }
      }
    }).find((f) => f != 0).getOrElse(0)
  }
}