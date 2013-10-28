package org.cddcore.engine

object Reportable {
  type ReportableList = List[Reportable]
  type ReportableSet = Set[Reportable]

}

case class UrlMap(val toUrl: Map[Reportable, String], val fromUrl: Map[String, List[Reportable]]) {
  def apply(r: Reportable): String = toUrl(r)
  def get(r: Reportable): Option[String] = toUrl.get(r)
  def apply(url: String) = fromUrl(url)
  def get(url: String) = fromUrl.get(url)
  def contains(r: Reportable) = toUrl.contains(r)
  def +(kv: (List[Reportable], String)) = UrlMap(toUrl + (kv._1.head -> kv._2), fromUrl + (kv._2 -> kv._1))
}

trait Reportable {
  def templateName: String = getClass.getSimpleName()
}

trait ReportableHolder extends Reportable with Traversable[Reportable] {
  import Reportable._
  def children: ReportableList
  def foreach[U](f: Reportable => U): Unit = {
    for (c <- children) c match { 
      case holder: ReportableHolder =>
        f(c); holder.foreach(f)
      case c => f(c)
    }
  }

  def walkWithPath(visitor: (ReportableList) => Unit): Unit = walkWithPath(List(), visitor)

  protected def walkWithPath(path: ReportableList, visitor: (ReportableList) => Unit): Unit = {
    val newPath = this :: path
    visitor(newPath)
    for (c <- children)
      c match {
        case holder: ReportableHolder => holder.walkWithPath(newPath, visitor)
        case _ => visitor(c :: newPath)
      }
  }

  def foldWithPath[Acc](path: ReportableList, initial: Acc,
    startFn: (Acc, ReportableList) => Acc,
    childFn: (Acc, ReportableList) => Acc,
    endFn: (Acc, ReportableList) => Acc): Acc = {
    val newPath = this :: path
    var acc = startFn(initial, newPath)
    for (c <- children)
      c match {
        case holder: ReportableHolder =>
          acc = holder.foldWithPath(newPath, acc, startFn, childFn, endFn)
        case _ =>
          acc = childFn(acc, c :: newPath)
      }
    acc = endFn(acc, newPath)
    acc
  }

  def foldWithPath[Acc](path: ReportableList, initial: Acc, fn: (Acc, ReportableList) => Acc): Acc = {
    val newPath = this :: path
    var acc = fn(initial, newPath)
    for (c <- children)
      c match {
        case holder: ReportableHolder =>
          acc = holder.foldWithPath(newPath, acc, fn)
        case _ => acc = fn(acc, c :: newPath)
      }
    acc
  }
}

trait RequirementAndHolder extends ReportableHolder with Requirement

trait Requirement extends Reportable {
  def title: Option[String]
  def titleString = title.getOrElse("")
  def titleOrDescription(default: String): String = title.getOrElse(description.getOrElse(default))

  def description: Option[String]
  def priority: Int
  def references: List[Reference]
}

trait Test extends Requirement {
  def optCode: Option[CodeHolder]
  def expected: Option[ROrException[_]]
  def because: Option[CodeHolder]

  def params: List[Any]
  def paramPrinter: LoggerDisplayProcessor
}

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