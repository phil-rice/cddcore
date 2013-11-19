package org.cddcore.engine

class NestedTraceBuilder(val params: List[Any], engine: Engine[_], items: List[TraceItem], val parent: TraceBuilder) extends TraceBuilder(items) {
  override def finished(conclusion: Conclusion, result: Any) = parent.copyWithNewItem(new TraceItem(engine, params, items, conclusion, ROrException(result), System.nanoTime() - startTime))
  override def failed(conclusion: Conclusion, exception: Throwable): TraceBuilder = parent.copyWithNewItem(new TraceItem(engine, params, items, conclusion, ROrException(exception), System.nanoTime() - startTime))
  override def copyWithNewItem(item: TraceItem) = new NestedTraceBuilder(params, engine, items :+ item, parent)
}

class TraceBuilder(val items: List[TraceItem]) {
  val startTime = System.nanoTime()
  def nest(e: Engine[_], params: List[Any]) = new NestedTraceBuilder(params, e, List(), this)
  def copyWithNewItem(item: TraceItem) = new TraceBuilder(items :+ item)
  def finished(conclusion: Conclusion, result: Any): TraceBuilder = throw new IllegalStateException;
  def failed(conclusion: Conclusion, exception: Throwable): TraceBuilder = throw new IllegalStateException;
}

case class TraceItem(engine: Engine[_], params: List[Any], nested: List[TraceItem], conclusion: Conclusion, result: ROrException[Any], took: Long) {
  override def hashCode = engine.hashCode() + params.hashCode
  override def equals(other: Any) =
    other match { case t: TraceItem => t.engine == engine && t.params == params && t.nested == nested && t.conclusion == conclusion && t.result == result; case _ => false }
} 
