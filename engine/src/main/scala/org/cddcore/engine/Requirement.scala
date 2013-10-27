package org.cddcore.engine

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

class SimpleReportableHolder(val children: List[Reportable]) extends ReportableHolder

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

