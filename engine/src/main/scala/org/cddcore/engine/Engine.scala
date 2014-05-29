package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions
import org.cddcore.utilities.Strings
import org.cddcore.utilities.TraceBuilder
import org.cddcore.utilities.CodeHolder
import org.cddcore.engine.builder._
import org.cddcore.utilities.CddDisplayProcessor
import org.cddcore.utilities.ExceptionMap
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Future
import org.cddcore.utilities.AnyTraceItem
import org.cddcore.utilities.TraceItem
import org.cddcore.utilities.Profiler
import org.cddcore.utilities.SimpleProfiler
import org.cddcore.utilities.ProfilerRecord

object TemplateLike {
  implicit object ReportableTemplateLike extends TemplateLike[Reportable] {
    def apply(r: Reportable) = r match {
      case rt: ReportableWithTemplate => rt.template
      case r => r.getClass().getSimpleName
    }
  }
}
trait TemplateLike[T] {
  def apply(t: T): String
}

trait Engine extends Reportable with Titled{
  def titleString: String
}

object EngineTools {
  implicit def toEngineTools[Params, BFn, R, RFn](e: Engine) = e.asInstanceOf[EngineTools[Params, BFn, R, RFn]]
}

trait EngineTools[Params, BFn, R, RFn] extends Engine with TypedReportable[Params, BFn, R, RFn] with WithCddDisplayProcessor {
  def title = asRequirement.title
  def asRequirement: EngineRequirement[Params, BFn, R, RFn]
  def evaluator: EvaluateTree[Params, BFn, R, RFn]
  def buildExceptions: ExceptionMap
  import ReportableHelper._
  lazy val exceptionsItCanThrow = asRequirement.scenarios.flatMap {
    (s: Scenario[Params, BFn, R, RFn]) =>
      s.expected match {
        case Some(Left(e)) => List[Class[_ <: Exception]](e.getClass())
        case _ => List[Class[_ <: Exception]]()
      }
  }.toSet
}

trait DelegatedEngine[Params, BFn, R, RFn] extends EngineTools[Params, BFn, R, RFn] {
  val delegate: EngineTools[Params, BFn, R, RFn]
  def asRequirement = delegate.asRequirement
  def evaluator = delegate.evaluator
  def buildExceptions = delegate.buildExceptions
  def ldp: CddDisplayProcessor = delegate.ldp
}

trait CachedEngine[Params, BFn, R, RFn, FullR] extends DelegatedEngine[Params, BFn, R, RFn] {
  protected val cache = new AtomicReference[Map[Params, Future[FullR]]](Map())
}

trait EngineRequirement[Params, BFn, R, RFn] extends BuilderNodeAndHolder[Params, BFn, R, RFn] with Requirement with TypedReportable[Params, BFn, R, RFn] {
  def requirementsIncludingTree(pathNotIncludingThis: List[Reportable]): List[List[Reportable]]
  def pathsIncludingTreeAndEngine(pathNotIncludingThis: List[Reportable]): List[List[Reportable]]
}

trait FoldingEngine[Params, BFn, R, RFn, FullR] extends HasExceptionMap[R, RFn] with EngineTools[Params, BFn, R, RFn] {
  def engines: List[EngineFromTests[Params, BFn, R, RFn]]
  def initialValue: CodeHolder[() => FullR]
  def foldingFn: (FullR, R) => FullR
  def applyParams(params: Params): FullR = {
    val monitor = Engine.currentMonitor
    monitor.call(this, params)
    try {
      val result = engines.foldLeft(initialValue.fn())((acc, e) => foldingFn(acc, e.applyParams(params)))
      monitor.finished[FullR](this, None, result)
      result
    } catch { case e: Exception => monitor.failed(this, None, e); throw e }
  }
}

trait EngineFromTests[Params, BFn, R, RFn] extends EngineTools[Params, BFn, R, RFn] {
  def tree: DecisionTree[Params, BFn, R, RFn]

  def applyParams(params: Params): R = {
    val monitor = Engine.currentMonitor
    monitor.call(this, params)
    val makeClosures = evaluator.makeClosures
    import makeClosures._
    val c = try {
      val bc = makeBecauseClosure(params)
      evaluator.findConclusion(tree, bc)
    } catch {
      case e: Exception =>
        monitor.failed(this, None, e)
        throw e
    }
    try {
      val result: R = makeResultClosure(params).apply(c.code.fn)
      monitor.finished[R](this, Some(c), result)
      result
    } catch {
      case e: Exception =>
        monitor.failed(this, Some(c), e)
        e match {
          case u: UndecidedException => throw new UndecidedException(u.getMessage(), u.params, this)
          case _ if exceptionsItCanThrow.find((ex) => ex.isAssignableFrom(e.getClass)).isDefined => throw e
          case _ => throw FailedToExecuteException(this, params, e)
        }
    }
  }
  def toString(indent: String, root: DecisionTreeNode[Params, BFn, R, RFn]): String = {
    root match {
      case d: Decision[Params, BFn, R, RFn] =>
        indent + "if(" + d.prettyString + ")\n" +
          toString(indent + " ", d.yes) +
          indent + "else\n" +
          toString(indent + " ", d.no)
      case c: Conclusion[Params, BFn, R, RFn] => indent + c.code.pretty + "\n";
    }
  }
  override def toString(): String = toString("", tree.root)

}

object EngineMonitor {
  def apply() = new NoEngineMonitor

}
trait EngineMonitor {
  def call[Params](e: Engine, params: Params)(implicit ldp: CddDisplayProcessor)
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: CddDisplayProcessor)
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: CddDisplayProcessor)
}

class NoEngineMonitor extends EngineMonitor {
  def call[Params](e: Engine, params: Params)(implicit ldp: CddDisplayProcessor) {}
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: CddDisplayProcessor) {}
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: CddDisplayProcessor) {}
}

class PrintlnEngineMonitor extends EngineMonitor {
  var depth = new AtomicInteger(0)
  private val indent = Strings.blanks(depth.get * 2)
  def call[Params](e: Engine, params: Params)(implicit ldp: CddDisplayProcessor) {
    println(Strings.oneLine(s"Calling:  $indent${e.titleString} with ${ldp(params)}"))
    depth.incrementAndGet()
  }
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: CddDisplayProcessor) {
    depth.decrementAndGet()
    println(s"Finished:  $indent ---> ${ldp(result)}")
  }
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: CddDisplayProcessor) {
    depth.decrementAndGet()
    println(Strings.oneLine(s"Failed:  $indent ---> ${ldp(exception)}"))
  }
}

class TraceEngineMonitor(implicit ldp: CddDisplayProcessor) extends EngineMonitor {
  var logging = true
  var traceBuilder = TraceBuilder[Engine, Any, Any, Conclusion[_, _, _, _]]()
  def log(prefix: String, stuff: => {}) = { stuff; if (logging) println(prefix + " " + Strings.oneLine(traceBuilder.shortToString)) }
  def call[Params](e: Engine, params: Params)(implicit ldp: CddDisplayProcessor) =
    log("call", traceBuilder = traceBuilder.nest(e.asInstanceOf[Engine], params))
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: CddDisplayProcessor) =
    log("finished", traceBuilder = traceBuilder.finished(result, conclusion))
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: CddDisplayProcessor) =
    log("failed", traceBuilder = traceBuilder.failed(exception, conclusion))
  def trace = traceBuilder.children
}

class ProfileEngineMonitor extends EngineMonitor {
  val profiler = new SimpleProfiler[Engine]
  def call[Params](e: Engine, params: Params)(implicit ldp: CddDisplayProcessor) = profiler.start(e)
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: CddDisplayProcessor) = profiler.end(e)
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: CddDisplayProcessor) = profiler.end(e)
}

class MultipleEngineMonitors(monitors: List[EngineMonitor]) extends EngineMonitor {
  def call[Params](e: Engine, params: Params)(implicit ldp: CddDisplayProcessor) = for (m <- monitors) m.call(e, params)
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: CddDisplayProcessor) = for (m <- monitors) m.finished(e, conclusion, result)
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: CddDisplayProcessor) = for (m <- monitors) m.failed(e, conclusion, exception)
}

object Engine {
  var logging = ("true" == System.getenv("cdd.junit.log")) || false
  /** returns a builder for an engine that implements Function[P,R] */
  def apply[P, R]()(implicit ldp: CddDisplayProcessor) = Builder1[P, R, R](BuildEngine.initialNodes, ExceptionMap(), BuildEngine.builderEngine1)(ldp)
  /** returns a builder for an engine that implements Function2[P1,P2,R] */
  def apply[P1, P2, R]()(implicit ldp: CddDisplayProcessor) = Builder2[P1, P2, R, R](BuildEngine.initialNodes, ExceptionMap(), BuildEngine.builderEngine2)(ldp)
  /** returns a builder for an engine that implements Function3[P1,P2,P3,R] */
  def apply[P1, P2, P3, R]()(implicit ldp: CddDisplayProcessor) = Builder3[P1, P2, P3, R, R](BuildEngine.initialNodes, ExceptionMap(), BuildEngine.builderEngine3)(ldp)

  def folding[P, R, FullR](foldingFn: (FullR, R) => FullR, initialValue: FullR)(implicit ldp: CddDisplayProcessor) =
    Builder1[P, R, FullR](BuildEngine.initialNodes[P, (P) => Boolean, R, (P) => R, FullR](initialValue, foldingFn), ExceptionMap(), BuildEngine.folderBuilderEngine1[P, R, FullR])(ldp)
  def foldList[P, R] = folding[P, R, List[R]]((acc: List[R], v: R) => acc :+ v, List())
  def foldSet[P, R] = folding[P, R, Set[R]]({ _ + _ }, Set())

  def folding[P1, P2, R, FullR](foldingFn: (FullR, R) => FullR, initialValue: FullR)(implicit ldp: CddDisplayProcessor) =
    Builder2[P1, P2, R, FullR](BuildEngine.initialNodes(initialValue, foldingFn), ExceptionMap(), BuildEngine.folderBuilderEngine2[P1, P2, R, FullR])(ldp)
  def foldList[P1, P2, R] = folding[P1, P2, R, List[R]]((acc: List[R], v: R) => acc :+ v, List())
  def foldSet[P1, P2, R] = folding[P1, P2, R, Set[R]]({ _ + _ }, Set())

  def folding[P1, P2, P3, R, FullR](foldingFn: (FullR, R) => FullR, initialValue: FullR)(implicit ldp: CddDisplayProcessor) =
    Builder3[P1, P2, P3, R, FullR](BuildEngine.initialNodes(initialValue, foldingFn), ExceptionMap(), BuildEngine.folderBuilderEngine3[P1, P2, P3, R, FullR])(ldp)
  def foldList[P1, P2, P3, R] = folding[P1, P2, P3, R, List[R]]((acc: List[R], v: R) => acc :+ v, List())
  def foldSet[P1, P2, P3, R] = folding[P1, P2, P3, R, Set[R]]({ _ + _ }, Set())

  def testing = _testing.get
  private var _testing = new ThreadLocal[Boolean] {
    override def initialValue = false;
  }
  def test[X](x: => X) = {
    _testing.set(true)
    try {
      x
    } finally
      _testing.set(false)
  }
  protected val defaultMonitor = new ThreadLocal[EngineMonitor] {
    override def initialValue = EngineMonitor()
  }

  def currentMonitor = defaultMonitor.get

  def withMonitor[X](m: EngineMonitor, fn: => X) = {
    val oldMonitor = defaultMonitor.get
    val newMonitor = new MultipleEngineMonitors(List(m, oldMonitor))
    defaultMonitor.set(newMonitor)
    try {
      fn
    } finally {
      defaultMonitor.set(oldMonitor)
    }
  }
  def trace[X](fn: => X)(implicit ldp: CddDisplayProcessor) = {
    val tm = new TraceEngineMonitor
    val result = try { Right(withMonitor(tm, fn)) } catch { case e: Exception => Left(e) }
    (result, tm.trace)
  }
  def traceNoException[X](fn: => X)(implicit ldp: CddDisplayProcessor) = {
    trace(fn) match {
      case (Left(e), _) => throw new RuntimeException("Exception in trace", e)
      case (Right(r), trace) => (r, trace)
    }
  }
  def profile[X](fn: => X) = {
    val pm = new ProfileEngineMonitor
    val result = try { Right(withMonitor(pm, fn)) } catch { case e: Exception => Left(e) }
    (result, pm.profiler)
  }
  def profileNoException[X](fn: => X) = {
    profile(fn) match {
      case (Left(e), _) => throw new RuntimeException("Exception in profile", e)
      case (Right(r), profiler) => (r, profiler)
    }
  }

}

