package org.cddcore.engine

import org.cddcore.engine.tests.CddRunner
import java.io.File

object Reportable {
  type ReportableList = List[Reportable]
  type ReportableSet = Set[Reportable]
  type UrlMap = Map[Reportable, String]

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

trait ReportableToUrl {
  import Reportable._
  protected var reqId = 0
  protected var cache = Map[Reportable, String]()
  protected var seen = Set[String]()

  /** Will return a human readable name for the reportable. Will allways return the same name for the reportable */
  def apply(r: Reportable): String = {
    val existing = cache.get(r)
    existing match {
      case Some(s) => s;
      case _ => {
        def makeNewName: String = {
          reqId += 1; val default = r.templateName + reqId;
          val result = Strings.urlClean(r match {
            case req: Requirement => { val result = req.titleOrDescription(default); if (result.length > 20) default else result }
            case _ => default;
          }).replace(" ", "_")
          if (seen.contains(result)) default else result
        }
        var result: String = null
        do {
          result = makeNewName
        } while (seen.contains(result))
        cache += (r -> result)
        seen += result
        result
      }
    }
  }

  /** Will return a human readable name for each reportable in the reversed list. Typically this is used to make a path */
  def apply(path: ReportableList, separator: String = "/"): String = path.reverse.map(apply(_)).mkString(separator)

  def url(path: ReportableList): Option[String]
  def makeUrlMap(r: ReportableHolder): Map[Reportable, String] =
    r.foldWithPath(List(), Map[Reportable, String](), ((acc: Map[Reportable, String], path) => {
      val u = url(path);
      if (u.isDefined) acc + (path.head -> u.get) else acc
    }))
}

class FileSystemReportableToUrl(val dir: File = CddRunner.directory) extends ReportableToUrl {
  import Reportable._
  def file(path: ReportableList) = new File(dir, apply(path, "\\") + "." + path.head.templateName + ".html")
  def url(path: ReportableList) = Some("file:///" + file(path).getAbsolutePath())
}

class NoReportableToUrl extends ReportableToUrl {
  import Reportable._
  def dir: File = CddRunner.directory
  def url(path: ReportableList) = None
  override def hashCode = 0
  override def equals(other: Any) = other != null && other.isInstanceOf[NoReportableToUrl]
}
