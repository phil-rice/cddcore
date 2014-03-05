package org.cddcore.engine

import scala.language.implicitConversions

object ConclusionOrResult {
  type ConclusionOrResult = Option[Conclusion]
  implicit def liftConclusion(c: Conclusion): ConclusionOrResult = Some(c)
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

case class TraceItem(engine: Engine, params: List[Any], nested: List[TraceItem], conclusion: ConclusionOrResult.ConclusionOrResult, result: ROrException[Any], took: Long) {
  def toString(loggerDisplayProcesser: LoggerDisplayProcessor, indent: String): String = s"${indent}TraceItem(${engine.titleString}, ${loggerDisplayProcesser(params)} => ${result}\n${nested.map(_.toString(loggerDisplayProcesser, indent + "  "))}"
  def toString(loggerDisplayProcesser: LoggerDisplayProcessor): String = toString(loggerDisplayProcesser, "")
  override def hashCode = engine.hashCode() + params.hashCode
  override def equals(other: Any) =
    other match { case t: TraceItem => t.engine == engine && t.params == params && t.nested == nested && t.conclusion == conclusion && t.result == result; case _ => false }
} 
