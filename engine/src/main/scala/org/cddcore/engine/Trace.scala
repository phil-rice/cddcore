package org.cddcore.engine

import scala.language.implicitConversions

import org.cddcore.reporting.HtmlRenderer
import org.cddcore.reporting.SimpleReportableToUrl
import org.cddcore.utilities.Strings

object ConclusionOrResult {
  type ConclusionOrResult = Option[Conclusion]
  implicit def liftConclusion(c: Conclusion): ConclusionOrResult = Some(c)
}

trait TraceItemVistor {
  def startItem(depth: Int, item: TraceItem)
  def endItem(depth: Int, item: TraceItem)
}

object TraceItem {
  def walk(item: TraceItem, visitor: TraceItemVistor) { walk(0, item, visitor) }

  private def walk(depth: Int, item: TraceItem, visitor: TraceItemVistor) {
    visitor.startItem(depth, item)
    for (i <- item.children)
      walk(depth + 1, i, visitor)
    visitor.endItem(depth, item)
  }

  def fold[Acc](initial: Acc, startFn: (Acc, Int, TraceItem) => Acc, endFn: (Acc, Int, TraceItem) => Acc, item: TraceItem): Acc =
    fold(initial, startFn, endFn, 0, item)

  private def fold[Acc](initial: Acc, startFn: (Acc, Int, TraceItem) => Acc, endFn: (Acc, Int, TraceItem) => Acc, depth: Int, item: TraceItem): Acc = {
    val afterStart = startFn(initial, depth, item)
    val afterChildren = item.children.foldLeft(afterStart)((acc: Acc, i: TraceItem) => fold[Acc](acc, startFn, endFn, depth + 1, i))
    val result = endFn(afterChildren, depth, item)
    result
  }

  def print(ldp: LoggerDisplayProcessor = LoggerDisplayProcessor())(item: TraceItem): String = fold[String]("",
    (acc: String, depth: Int, i: TraceItem) => acc + "\n" + Strings.blanks(depth) + i.engine.titleString + "(" + i.params.map(ldp(_)).mkString(",") + ") => " + ldp(i.result) + " ... " + i.took,
    (acc: String, depth: Int, i: TraceItem) => acc,
    item)

  def html(ldp: LoggerDisplayProcessor = LoggerDisplayProcessor())(items: TraceItem*): String = {
    val report = Report("Trace", items: _*)
    val reportableToUrl = new SimpleReportableToUrl
    val urlMap = reportableToUrl.makeUrlMap(report)
    HtmlRenderer.apply(ldp, false).traceHtml(None).render(reportableToUrl, urlMap, report)
  }

}

class NestedTraceBuilder(val params: List[Any], engine: Engine, items: List[TraceItem], val parent: TraceBuilder) extends TraceBuilder(items, parent.ignore) {
  import ConclusionOrResult._
  override def finished(conclusion: ConclusionOrResult, result: Any) = parent.copyWithNewItem(new TraceItem(engine, params, items, conclusion, ROrException(result), System.nanoTime() - startTime))
  override def failed(conclusion: ConclusionOrResult, exception: Throwable): TraceBuilder = parent.copyWithNewItem(new TraceItem(engine, params, items, conclusion, ROrException(exception), System.nanoTime() - startTime))
  override def copyWithNewItem(item: TraceItem) = new NestedTraceBuilder(params, engine, items :+ item, parent)
}

class IgnoreTraceBuilder(val parent: TraceBuilder) extends TraceBuilder(List(), parent.ignore) {
  import ConclusionOrResult._
  override def finished(conclusion: ConclusionOrResult, result: Any) = parent
  override def failed(conclusion: ConclusionOrResult, exception: Throwable): TraceBuilder = parent
  override def copyWithNewItem(item: TraceItem) = this
}

class TraceBuilder(val items: List[TraceItem], val ignore: List[Engine]) {
  import ConclusionOrResult._
  val startTime = System.nanoTime()
  def nest(e: Engine, params: List[Any]) = if (ignore.contains(e)) new IgnoreTraceBuilder(this) else new NestedTraceBuilder(params, e, List(), this)
  def copyWithNewItem(item: TraceItem) = new TraceBuilder(items :+ item, ignore)
  def finished(conclusion: ConclusionOrResult, result: Any): TraceBuilder = throw new IllegalStateException;
  def failed(conclusion: ConclusionOrResult, exception: Throwable): TraceBuilder = throw new IllegalStateException;
}

case class TraceItem(engine: Engine, params: List[Any], children: List[TraceItem], conclusion: ConclusionOrResult.ConclusionOrResult, result: ROrException[Any], took: Long) extends ReportableHolder {
  def toString(loggerDisplayProcesser: LoggerDisplayProcessor, indent: String): String = s"${indent}TraceItem(${engine.titleString}, ${loggerDisplayProcesser(params)} => ${result}\n${children.map(_.toString(loggerDisplayProcesser, indent + "  "))}"
  def toString(loggerDisplayProcesser: LoggerDisplayProcessor): String = toString(loggerDisplayProcesser, "")
  override def hashCode = engine.hashCode() + params.hashCode
  override def equals(other: Any) =
    other match { case t: TraceItem => t.engine == engine && t.params == params && t.children == children && t.conclusion == conclusion && t.result == result; case _ => false }
} 
