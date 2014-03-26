package org.cddcore.engine

import scala.language.implicitConversions
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Future

object Reportable {
  type ReportableList = List[Reportable]
  type ReportableSet = Set[Reportable]

  def allTests(list: List[Reportable]): List[Test] = list.flatMap(_ match { case t: Test => List(t); case rh: ReportableHolder => allTests(rh.children) })
  val priorityOrdering = new Ordering[Reportable]() {
    def compare(r1: Reportable, r2: Reportable) = {
      def priority(r: Reportable) = r match { case r: Requirement => r.priority.getOrElse(0); case _ => 0 }
      val p1 = priority(r1)
      val p2 = priority(r2)
      p1 - p2
    }
  }
  def textOrder(r: Any) = r match {
    case Some(r: ReportableWithTextOrder) => r.textOrder
    case r: ReportableWithTextOrder => r.textOrder
    case _ => 0
  }

  def unwrap[R <: Reportable](r: R): R = r match {
    case w: ReportableWrapper => w.delegate.collect { case r => unwrap(r) }.getOrElse(r).asInstanceOf[R]
    case _ => r
  }

  def documentsIn(r: Reportable): Set[Document] = {
    def fromRequirement(r: Requirement) = r.references.map(_.document).collect { case Some(d) => d }
    def fromHolder(r: ReportableHolder) = r.children.foldLeft[Set[Document]](Set())(_ ++ documentsIn(_))
    val result = r match {
      case r: RequirementAndHolder => fromRequirement(r) ++ fromHolder(r)
      case r: Requirement => fromRequirement(r)
      case r: ReportableHolder => fromHolder(r)
    }
    result.toSet
  }
  def templateName(a: Any): String = a match {
    case l: List[_] => Reportable.templateName(l.head);
    case r: ReportableWrapper => if (r.delegate.isDefined) templateName(r.delegate.get) else r.getClass().getSimpleName();
    case r: ReportableWithTemplate => r.templateName;
    case _ => a.getClass.getSimpleName()
  }
  def templateNameAndTitle(a: Any): String = templateName(a) + "(" + (a match {
    case r: Requirement => r.titleString
    case m: MergedShortDescription => m.name
    case _ => a.toString
  }) + ")"

}

/** Reportables are things that appear in reports. This includes Engines, Use cases and tests*/
trait Reportable

/** A reportable with a wrapper is used in the reports when making a projection of an original report. This allows the correct urls to be determined */
trait ReportableWrapper extends Reportable {
  def delegate: Option[Reportable]

}
object ReportableWithTemplate {
  implicit def toReportableWithTemplate(r: Reportable) = r.asInstanceOf[ReportableWithTemplate]
}

/** Templates was used to control the text is generated. This is used when the class simple name isn't good enough for template selection */
trait ReportableWithTemplate {
  def templateName: String
}

/** Classes implementing this are displayed using htmlDisplay, rather than toString when shown on a website, or in HTML pages.*/
trait HtmlDisplay {
  def htmlDisplay: String
}

/**
 * Quite a lot of reportables have 'child' reportables. Engines have usecases or scenarios. Scenarios have children. This trait requires the children method to be implemented, hten provides a number of common filtering, walking and folding operations
 *  Note that this implements 'foreach' so using this as a traversable visits all the descendants in a depth first manner
 */
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

  /** All the descendants that implement the class. Doesn't include 'this' */
  def all[R <: Reportable](rClass: Class[R]) = allReversed(rClass).reverse
  def allReversed[R <: Reportable](rClass: Class[R]) = foldLeft[List[R]](List())((acc, r) => if (rClass.isAssignableFrom(r.getClass)) r.asInstanceOf[R] :: acc else acc)

  /** Calls this visitor with 'this' and all it's descendents. The parameter to visitor is the path from the head back up to 'this'. So head will be the deepest reportable */
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
  /** folds across 'this' and all it's descendents. The folding function takes the accumulator and the path from the descendant up to 'this' to return a new accumulator */
  def foldWithPath[Acc](initial: Acc, fn: (Acc, ReportableList) => Acc): Acc = foldWithPath(List(), initial, fn)

  private def foldWithPath[Acc](path: ReportableList, initial: Acc, fn: (Acc, ReportableList) => Acc): Acc = {
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

  /** folds across 'this' and all it's descendents. The folding function takes the accumulator and the path from the descendant up to 'this' to return a new accumulator  In this folding operation though, all the children are reversed.   */
  def foldWithPathReversingChildren[Acc](initial: Acc, fn: (Acc, ReportableList) => Acc): Acc = foldWithPathReversingChildren(List(), initial, fn)
  private def foldWithPathReversingChildren[Acc](path: ReportableList, initial: Acc, fn: (Acc, ReportableList) => Acc): Acc = {
    val newPath = this :: path
    var acc = fn(initial, newPath)
    for (c <- children.reverse)
      c match {
        case holder: ReportableHolder =>
          acc = holder.foldWithPathReversingChildren(newPath, acc, fn)
        case _ => acc = fn(acc, c :: newPath)
      }
    acc
  }
}

/** Paths are often used: see ReportableAndHolder walking and folding operations. A path is a List of Reportables in which the deepest item is the head of the list, and the top container is the last item in the list. PathUtils provides ways of getting information out of the path */
object PathUtils {
  import Reportable._

  /** Walks up the path until it finds the first use case */
  def findUseCase(path: ReportableList) = findUseCasePath(path).head.asInstanceOf[UseCase]
  /** Walks up the path until it finds the first use case, return a truncated path with the usecase as the head */
  def findUseCasePath(path: ReportableList): ReportableList = path match {
    case (usecase: UseCase) :: tail => path
    case h :: tail => findUseCasePath(tail)
    case _ => throw new IllegalArgumentException
  }

  /** Walks up the path until it finds the first engine with tests*/
  def findEngineWithTests(path: ReportableList) = {
    val newPath = engineWithTestsPath(path)
    newPath.head.asInstanceOf[EngineBuiltFromTests[_]]
  }
  /** Walks up the path until it finds the first engine with tests, return a truncated path with the engine as the head */
  def engineWithTestsPath(path: ReportableList): ReportableList = path match {
    case (engine: EngineBuiltFromTests[_]) :: tail => path
    case (engine: DelegatedEngine) :: tail => engineWithTestsPath(engine.delegate :: tail)
    case h :: tail => engineWithTestsPath(tail)
    case _ => throw new IllegalArgumentException
  }
  /** Walks up the path until it finds the first engine*/
  def findEngine(path: ReportableList) = enginePath(path).head.asInstanceOf[Engine]
  /** Walks up the path until it finds the first engine, return a truncated path with the engine as the head */
  def enginePath(path: ReportableList): ReportableList = path match {
    case (engine: Engine) :: tail => path
    case h :: tail => enginePath(tail)
    case _ => throw new IllegalArgumentException
  }
  /** Walks up the path until it finds the first engine*/
  def findEnginePathIfExists(path: ReportableList) = enginePathIfExists(path).headOption.collect { case e: Engine => e }
  /** Walks up the path until it finds the first engine, return a truncated path with the engine as the head */
  def enginePathIfExists(path: ReportableList): ReportableList = path match {
    case (engine: Engine) :: tail => path
    case h :: tail => enginePathIfExists(tail)
    case _ => List()
  }

  /** Walks up the path until it finds the first project */
  def findProject(path: ReportableList) = projectPath(path).head.asInstanceOf[Project]
  /** Walks up the path until it finds the first project, return a truncated path with the project as the head */
  def projectPath(path: ReportableList): ReportableList = path match {
    case (project: Project) :: tail => path
    case h :: tail => projectPath(tail)
    case _ => throw new IllegalArgumentException
  }
  /** Walks up the path until it finds the first report*/
  def findReport(path: ReportableList) = reportPath(path).head.asInstanceOf[Report]
  /** Walks up the path until it finds the first report, return a truncated path with the report as the head */
  def reportPath(path: ReportableList): ReportableList = path match {
    case (project: Report) :: tail => path
    case h :: tail => reportPath(tail)
    case _ => throw new IllegalArgumentException
  }

  /** returns the maximum priority or None*/
  def maxPriority(path: ReportableList): Option[Int] = path.foldLeft[Option[Int]](None)((acc, r: Reportable) =>
    (acc, r) match {
      case (None, (r: Requirement)) => r.priority
      case (Some(p), r: Requirement) if (r.priority.isDefined && r.priority.get > p) => Some(p);
      case (acc, _) => acc
    });

  def findTraceItem(path: ReportableList): TraceItem = path match {
    case (traceItem: TraceItem) :: tail => traceItem
    case h :: tail => findTraceItem(tail)
    case _ => throw new IllegalArgumentException
  }
}

/** Very little is known about Reportables: no methods are implemented. Requirements are the main Reportable. Requirements include Engines, UseCases and Scenarios */
trait Requirement extends Reportable {
  /** this will appear on reports as a title for that requirement*/
  def title: Option[String]
  /** the title or an empty string*/
  def titleString = title.getOrElse("")
  /** the title or if undefined the description or if undefined the default*/
  def titleOrDescription(default: String): String = title.getOrElse(description.getOrElse(default))

  /** An optional description*/
  def description: Option[String]
  /** The 'priority' that is used to determine the order that the decision trees are being built */
  def priority: Option[Int]

  def references: Set[Reference]
}

trait ReportableWithTextOrder extends Reportable {
  def textOrder: Int
}

/** As the name suggests this is both a requirement and a reportableholder */
trait RequirementAndHolder extends ReportableHolder with Requirement

/** Usecases are at the moment mostly 'just text'. They are effectively a grouping mechanism, to group tests */
trait UseCase extends RequirementAndHolder with ReportableWithTextOrder {
  /** The default 'code' for scenario's underneath it. It is very common for all things implementing a use case to use the same code block, and using this makes that clear */
  def optCode: Option[AbstractCodeHolder]
  /** The default expected for scenario's underneath it. It is very common for all things implementing a use case to come to the same conclusion, and using this avoids duplication and makes it clear */
  def expected: Option[ROrException[_]]
}

/** This is a full 'test + optional reason why + optional code to implement the behaviour that will make the test past */
trait Test extends Requirement with ReportableWithTextOrder {
  /** The inputs for the test. The test is mostly saying 'if I pass the engine these parameters, then I expect this result*/
  def params: List[Any]
  /** The expected output for the test. The test is mostly saying 'if I pass the engine these parameters, then I expect this result*/
  def expected: Option[ROrException[_]]
  /** The because is a tool that the engine uses to come to the correct conclusions. A good because function should have no sideeffects and execute quickly*/
  def because: Option[AbstractCodeHolder]
  /** If you want to display the because, this returns the description or an empty string if none is defined */
  def becauseString = because match { case Some(b) => b.description; case _ => "" }
  /** The code for the scenario. This is optional as if the result is 'the expected' then the code is redundant */
  def optCode: Option[AbstractCodeHolder]
}

/** A report is what it says: the object representation of a (usually) html report */
case class Report(reportTitle: String, reportables: Reportable*) extends RequirementAndHolder {
  val title = Some(reportTitle)
  val children = reportables.toList
  val description = None
  val priority = None
  val references = Set[Reference]()
}

/** A project is basically a list of the engines that are used on a software project */
case class Project(projectTitle: String, engines: ReportableHolder*) extends RequirementAndHolder {
  val title = Some(projectTitle)
  val children = engines.toList
  def description = None
  def priority = None
  def references = Set()
}

object Engine {
  import ConclusionOrResult._
  val engineCount = new AtomicInteger(0)

  protected def addToList[R] = (acc: List[R], r: R) => r :: acc
  protected def addToSet[R] = (acc: Set[R], r: R) => acc + r
  protected def addOption[R] = (acc: List[R], optR: Option[R]) => optR match { case Some(r) => r :: acc; case None => acc; }
  protected def addList[R] = (acc: List[R], listR: List[R]) => listR ::: acc
  val noInitialValue = () => throw new IllegalStateException
  /** returns a builder for an engine that implements Function[P,R] */
  def apply[P, R]() = new BuilderFactory1[P, R, R](None, noInitialValue).builder;
  /** returns a builder for an engine that implements Function2[P1,P2,R] */
  def apply[P1, P2, R]() = new BuilderFactory2[P1, P2, R, R](None, noInitialValue).builder;
  /** returns a builder for an engine that implements Function3[P1,P2,P3,R] */
  def apply[P1, P2, P3, R]() = new BuilderFactory3[P1, P2, P3, R, R](None, noInitialValue).builder;

  /** returns a builder for an engine that implements Function[P,FullR. This builder will have child engines that implement Function[P,R]. The folding function and initial value are used to produce the final result of the engine from the child engines */
  def folding[P, R, FullR](foldFn: (FullR, R) => FullR, initialValue: => FullR) = new BuilderFactory1[P, R, FullR](Some(foldFn), () => initialValue).builder
  /** returns a builder for an engine that implements Function2[P1,P2,FullR. This builder will have child engines that implement Function2[P1,P2,R]. The folding function and initial value are used to produce the final result of the engine from the child engines */
  def folding[P1, P2, R, FullR](foldFn: (FullR, R) => FullR, initialValue: => FullR) = new BuilderFactory2[P1, P2, R, FullR](Some(foldFn), () => initialValue).builder
  /** returns a builder for an engine that implements Function3[P1,P2,P3,FullR. This builder will have child engines that implement Function3[P1,P2,P3,R]. The folding function and initial value are used to produce the final result of the engine from the child engines */
  def folding[P1, P2, P3, R, FullR](foldFn: (FullR, R) => FullR, initialValue: => FullR) = new BuilderFactory3[P1, P2, P3, R, FullR](Some(foldFn), () => initialValue).builder

  /** returns a builder for an engine that implements Function[P,List[R]]. This builder will have child engines that implement Function[P,R]. The results of those engines are placed in a list, and become the result of the main engine*/
  def foldList[P, R] = folding[P, R, List[R]](addToList[R], List[R]())
  /** returns a builder for an engine that implements Function2[P1,P2,List[R]]. This builder will have child engines that implement Function2[P1,P2,R]. The results of those engines are placed in a list, and become the result of the main engine*/
  def foldList[P1, P2, R] = folding[P1, P2, R, List[R]](addToList[R], List[R]())
  /** returns a builder for an engine that implements Function3[P1,P2,P3,List[R]]. This builder will have child engines that implement Function[P1,P2,P3,R]. The results of those engines are placed in a list, and become the result of the main engine*/
  def foldList[P1, P2, P3, R] = folding[P1, P2, P3, R, List[R]](addToList[R], List[R]())

  /** returns a builder for an engine that implements Function[P,List[R]]. This builder will have child engines that implement Function[P,R]. The results of those engines are placed in a set, and become the result of the main engine*/
  def foldSet[P, R] = folding[P, R, Set[R]](addToSet[R], Set[R]())
  /** returns a builder for an engine that implements Function2[P1,P2,List[R]]. This builder will have child engines that implement Function2[P1,P2,R]. The results of those engines are placed in a set, and become the result of the main engine*/
  def foldSet[P1, P2, R] = folding[P1, P2, R, Set[R]](addToSet[R], Set[R]())
  /** returns a builder for an engine that implements Function3[P1,P2,P3,List[R]]. This builder will have child engines that implement Function[P1,P2,P3,R]. The results of those engines  are placed in a set, and become the result of the main engine*/
  def foldSet[P1, P2, P3, R] = folding[P1, P2, P3, R, Set[R]](addToSet[R], Set[R]())

  def flatMapOption[P, R] = folding[P, Option[R], List[R]](addOption[R], List())
  def flatMapOption[P1, P2, R] = folding[P1, P2, Option[R], List[R]](addOption[R], List())
  def flatMapOption[P1, P2, P3, R] = folding[P1, P2, P3, Option[R], List[R]](addOption[R], List())

  def flatMapList[P, R] = folding[P, List[R], List[R]](addList[R], List())
  def flatMapList[P1, P2, R] = folding[P1, P2, List[R], List[R]](addList[R], List())
  def flatMapList[P1, P2, P3, R] = folding[P1, P2, P3, List[R], List[R]](addList[R], List())

  /** returns a builder for an engine that takes S and P and returns the tuple (newS, R). It is typically used when there is a state of type S that needs to be updated with the function */
  def state[S, P, R]() = new BuilderFactory2[S, P, (S, R), (S, R)](None, noInitialValue).builder;
  /** returns a builder for an engine that takes S, P1, P2 and returns the tuple (newS, R). It is typically used when there is a state of type S that needs to be updated with the function */
  def state[S, P1, P2, R]() = new BuilderFactory3[S, P1, P2, (S, R), (S, R)](None, noInitialValue).builder;

  private var _traceBuilder: ThreadLocal[Option[TraceBuilder]] = new ThreadLocal[Option[TraceBuilder]] {
    override def initialValue = None
  }

  private var profiler = new ThreadLocal[Option[Profiler[Engine]]] {
    override def initialValue = None
  }

  def call(e: Engine, params: List[Any]) = {
    _traceBuilder.get match {
      case Some(tb) => _traceBuilder.set(Some(tb.nest(e, params)))
      case None =>
    }
    profiler.get() match {
      case Some(p) => p.start(e)
      case None =>
    }
  }

  def endCall(e: Engine, conclusion: ConclusionOrResult, result: Any) {
    _traceBuilder.get match {
      case Some(tb) => _traceBuilder.set(Some(tb.finished(conclusion, result)))
      case None =>
    }
    profiler.get() match {
      case Some(p) => p.end(e)
      case None =>
    }
  }
  def failedCall(e: Engine, conclusion: ConclusionOrResult, exception: Throwable) {
    _traceBuilder.get match {
      case Some(tb) => _traceBuilder.set(Some(tb.failed(conclusion, exception)))
      case None =>
    }
    profiler.get() match {
      case Some(p) => p.end(e)
      case None =>
    }
  }
  def profile[T](x: => T, p: Profiler[Engine] = new SimpleProfiler[Engine]): (T, Profiler[Engine]) = {
    profiler.set(Some(p))
    try {
      (x, p)
    } finally {
      profiler.set(None)
    }
  }

  def trace[T](x: => T, ignore: List[Engine] = List()): Tuple2[ROrException[T], List[TraceItem]] = {
    try {
      _traceBuilder.set(Some(new TraceBuilder(List(), ignore)));
      try {
        val result = x
        (ROrException(result), _traceBuilder.get.get.items)
      } catch { case e: Throwable => (ROrException(e), _traceBuilder.get.get.items) }
    } finally
      _traceBuilder.set(None);
  }
  var logging = ("true" == System.getenv("cdd.junit.log")) || false

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

}

/** The details about the parameters to an engine, including a parser for the live website and a default string value that allows the live website to be be tested by a selenium test easily*/
case class ParamDetail(displayName: String, parser: (String) => _, testValue: Option[String])

/** This is the visitor when folding a decision tree. */
trait DecisionTreeFolder[Acc] {
  def apply(acc: Acc, c: Conclusion): Acc
  def apply(acc: Acc, d: Decision): Acc

}

/** This is the core object that describes an Engine. */
trait Engine extends RequirementAndHolder with ReportableWithTextOrder {
  /** How many parameters the engine takes. Engine1[P,R] has 1, Engine2[P1,P2,R] has 2 */
  def arity: Int
  def decisionTreeNodes: Int
}

trait EngineWithConstructionString extends Engine {
  def constructionString: String
}
/** Engines have loggers which record interesting events. However to avoid polluting the API with them, if you absolutely need access to the logger you can import EngineWithLogger._ */
object EngineWithLogger {
  implicit def toEngineWithLogger(e: Engine) = e.asInstanceOf[EngineWithLogger]
}
trait EngineWithLogger extends Engine {
  /** used to meter the engine. It also doubles as a LoggerDisplayProcessor*/
  def logger: TddLogger
}

case class ScenarioExceptionMap(map: Map[Test, Throwable] = Map(), first: Option[Throwable] = None) {
  def size = map.size
  def values = map.values
  def +(x: (Test, Throwable)) = ScenarioExceptionMap(map + x, Some(first.getOrElse(x._2)))
  def +(s: ScenarioExceptionMap) = ScenarioExceptionMap(map ++ s.map, first match { case Some(f) => first; case _ => s.first })
  def apply(s: Test) = map(s)
  def contains(s: Test) = map.contains(s)
}

/** Engines have loggers which record interesting events. However to avoid polluting the API with them, if you absolutely need access to the logger you can import EngineWithLogger._ */
object EngineWithScenarioExceptionMap {
  implicit def toEngineWithScenarioExceptionMap(e: Engine) = e.asInstanceOf[EngineWithScenarioExceptionMap]
}
trait EngineWithScenarioExceptionMap extends Engine {
  def scenarioExceptionMap: ScenarioExceptionMap
}

trait EngineWithResult[R] extends Engine {
  /** This is used when you don't know at compile time the types and arity of the parameters. For example by the live website. Normally the apply method would be used, which is type safe */
  def applyParams(params: List[Any]): R
  def safeApplyParams(params: List[Any]) = try {
    ROrException(applyParams(params))
  } catch { case e: Throwable => ROrException(e) }
}

/** This is the 'normal' engine. It */
trait EngineBuiltFromTests[R] extends EngineWithResult[R] {
  import ConclusionOrResult._
  def root: Either[Conclusion, Decision]
  lazy val tests = all(classOf[Test])

  def evaluateBecauseForDecision(decision: Decision, params: List[Any]): Boolean
  def findConclusionFor(params: List[Any]): Conclusion
  def evaluateConclusion(params: List[Any], conclusion: Conclusion): R
  def evaluateConclusionNoException(params: List[Any], conclusion: Conclusion): ROrException[R]
  def applyParams(params: List[Any]): R = {
    Engine.call(this, params)
    val conclusion = findConclusionFor(params)
    val result = evaluateConclusion(params, conclusion)
    Engine.endCall(this, conclusion, result);
    result
  }

  def toString(indent: String, root: Either[Conclusion, Decision]): String = {
    root match {
      case null => indent + "null"
      case Left(result) => indent + result.code.pretty + "\n"
      case Right(node) =>
        indent + "if(" + node.prettyString + ")\n" +
          toString(indent + " ", node.yes) +
          indent + "else\n" +
          toString(indent + " ", node.no)
    }
  }
  override def toString(): String = toString("", root)

  def toStringWithScenarios(): String = toStringWithScenarios(root);

  def fold[Acc](initialValue: Acc, folder: DecisionTreeFolder[Acc]): Acc = fold[Acc](initialValue, root, folder)
  def fold[Acc](initialValue: Acc, root: Either[Conclusion, Decision], folder: DecisionTreeFolder[Acc]): Acc = root match {
    case Right(d: Decision) => fold(fold(folder(initialValue, d), d.yes, folder), d.no, folder)
    case Left(c: Conclusion) => folder(initialValue, c)
  }

  def walkDecisionsAndConclusion(fn: (ConclusionOrDecision) => Unit): Unit = walkDecisionsAndConclusion(root, fn)
  def walkDecisionsAndConclusion(root: Either[Conclusion, Decision], fn: (ConclusionOrDecision) => Unit): Unit = {
    root match {
      case Right(d: Decision) => fn(d); walkDecisionsAndConclusion(d.yes, fn); walkDecisionsAndConclusion(d.no, fn);
      case Left(c: Conclusion) => fn(c)
    }
  }
  def toStringWith(path: List[Reportable], root: Either[Conclusion, Decision], printer: IfThenPrinter): String =
    printer.start(path, this) + toStringPrimWith(path, root, printer) + printer.end

  def toStringWith(printer: IfThenPrinter): String = toStringWith(List(this), root, printer)

  private def toStringPrimWith(path: List[Reportable], root: Either[Conclusion, Decision], printer: IfThenPrinter): String = {
    root match {
      case null => "Could not toString as root as null. Possibly because of earlier exceptions"
      case Left(result) => printer.resultPrint(path, result)
      case Right(node: Reportable) =>
        val ifString = printer.ifPrint(path, node)
        val yesString = toStringPrimWith(path :+ node, node.yes, printer)
        val elseString = printer.elsePrint(path, node)
        val noString = toStringPrimWith(path :+ node, node.no, printer)
        val endString = printer.endPrint(path, node)
        val result = ifString + yesString + elseString + noString + endString
        return result
    }
  }

  def toStringWithScenarios(root: Either[Conclusion, Decision]): String =
    toStringWith(List(), root, new DefaultIfThenPrinter())

}

/** A child engine is an engine that 'belongs' to a full engine. Typically the results of the child engines (if they exist) will be 'folded' using a folding function to give the full result */
trait ChildEngine[R] extends EngineBuiltFromTests[R] with ReportableWithTemplate {
  def templateName = Renderer.engineChildKey
}

/** In order to avoid polluting the Engine trait, if you need access to the paramDetails, this can be imported */
object ParamDetails {
  implicit def toParamDetails(e: Engine) = e.asInstanceOf[ParamDetails]
}

trait ParamDetails {
  def paramDetails: List[ParamDetail]
}

/** An engine with child engines and a folding function for aggregating the results */
trait EngineFull[R, FullR] extends EngineWithResult[FullR] {
  import Reportable._
  import ConclusionOrResult._
  def documents: List[Document]
  //  def root: Either[Conclusion, Decision]
  //  protected def toStringWith(path: ReportableList, root: Either[Conclusion, Decision], printer: IfThenPrinter): String

  lazy val childEngines: List[ChildEngine[R]] = all(classOf[ChildEngine[R]])
  def initialFoldValue: () => FullR
  def folder: Option[(FullR, R) => FullR]
  def applyParams(params: List[Any]): FullR = {
    Engine.call(this, params)
    try {
      folder match {
        case Some(f) =>
          val result = childEngines.foldLeft[FullR](initialFoldValue())((acc, ce) =>
            f(acc, ce.applyParams(params)))
          Engine.endCall(this, None, result)
          result
        case _ => throw new IllegalStateException
      }
    } catch {
      case e: Throwable => {
        Engine.failedCall(this, None, e)
        throw e
      }
    }
  }
  lazy val decisionTreeNodes = childEngines.foldLeft(0)(_ + _.decisionTreeNodes)
}

trait DelegatedEngine extends Engine with EngineWithScenarioExceptionMap with EngineWithLogger {
  def delegate: Engine
  def decisionTreeNodes = delegate.decisionTreeNodes
  def children = delegate.children
  def textOrder = delegate.textOrder
  def title = delegate.title
  def description = delegate.description
  def priority = delegate.priority
  def references = delegate.references
  def scenarioExceptionMap: ScenarioExceptionMap = delegate.asInstanceOf[EngineWithScenarioExceptionMap].scenarioExceptionMap
  def logger: TddLogger = delegate.asInstanceOf[EngineWithLogger].logger
}

trait EngineCache[Params, R] {
  def getOrCreate(p: Params, r: => R): R
}

class CachedEngine1[P, R](val delegate: Engine1[P, R]) extends Engine1[P, R] with DelegatedEngine {
  private val cache = new AtomicReference[Map[P, Future[R]]](Map())
  def apply(p: P) = Maps.getOrCreate(cache, p, delegate(p))

}

trait Engine1[P, R] extends Engine with Function1[P, R] {
  def arity = 1
  def cached = new CachedEngine1[P, R](this);
}
class CachedEngine2[P1, P2, R](val delegate: Engine2[P1, P2, R]) extends Engine2[P1, P2, R] with DelegatedEngine {
  private val cache = new AtomicReference[Map[(P1, P2), Future[R]]](Map())
  def apply(p1: P1, p2: P2) = Maps.getOrCreate(cache, (p1, p2), delegate(p1, p2))
}

trait Engine2[P1, P2, R] extends Engine with Function2[P1, P2, R] {
  def arity = 2
  def cached = new CachedEngine2[P1, P2, R](this)
}
class CachedEngine3[P1, P2, P3, R](val delegate: Engine3[P1, P2, P3, R]) extends Engine3[P1, P2, P3, R] with DelegatedEngine {
  private val cache = new AtomicReference[Map[(P1, P2, P3), Future[R]]](Map())
  def apply(p1: P1, p2: P2, p3: P3) = Maps.getOrCreate(cache, (p1, p2, p3), delegate(p1, p2, p3))
}

trait Engine3[P1, P2, P3, R] extends Engine with Function3[P1, P2, P3, R] {
  def arity = 3
  private def outerEngine = this
  def cached = new Engine3[P1, P2, P3, R] with DelegatedEngine {
    private val cache = new AtomicReference[Map[(P1, P2, P3), Future[R]]](Map())
    def delegate = outerEngine
    def apply(p1: P1, p2: P2, p3: P3) = Maps.getOrCreate(cache, (p1, p2, p3), outerEngine(p1, p2, p3))
  }
}

/** The decision tree inside an engine with tests is made of nodes that either a conclusion or a decision. Both conclusion and decision extend this*/
trait ConclusionOrDecision extends Reportable {
  /** All the conclusions under or including this node in the decision tree*/
  def allConclusion: Set[Conclusion]
  protected def allConclusion(either: Either[Conclusion, Decision]): Set[Conclusion] =
    either match {
      case Left(c) => c.allConclusion
      case Right(d) => d.allConclusion
    }
}

/** This represents a decision in the decision tree*/
trait Decision extends ConclusionOrDecision {
  /** The condition that be evaluated. Usually there will be only one item in the list. Because of the idea of 'node merging': see EngineOrTests, this is a list. Effectively the condition is true if any of the becauses are true */
  def because: List[AbstractCodeHolder]
  /** If the because is true, then go this way*/
  def yes: Either[Conclusion, Decision]
  /** If the because is false, then go this way*/
  def no: Either[Conclusion, Decision]
  /** Some text to display the because with */
  def prettyString: String
  /** All the conclusions under (and including) the yes branch */
  lazy val allYesConclusion = allConclusion(yes)
  /** All the conclusions under (and including) the no branch */
  lazy val allNoConclusion = allConclusion(no)
  lazy val allConclusion: Set[Conclusion] = allYesConclusion ++ allNoConclusion
}

/** This represents a conclusion in the decision tree*/
trait Conclusion extends ConclusionOrDecision {
  /** The code that will be executed, should this node be reached. This will return the 'result' */
  def code: AbstractCodeHolder
  /** All the scenarios that 'end up' coming to the conclusion this node represents */
  def scenarios: List[Test]
  val allConclusion = Set(this)
}

/** Documents are external documents such as requirements specifications, emails and so on. The are linked to scenarios, usecases and engines by references */
case class Document(title: Option[String] = None, description: Option[String] = None, priority: Option[Int] = None, url: Option[String] = None, mergeStrategy: DocumentMergeStrategy = DocumentMergeStrategy.default) extends Requirement {
  def references = Set()
  override def titleString = title.getOrElse(titleOrDescription("<Unnamed>"))
}

trait DocumentMergeStrategy {

  def toRequirementAndEngine(r: Reportable, e: Option[Engine]): Option[RequirementAndEngine]
  def merge(key: String, list: List[RequirementAndEngine]): Reportable
}

class DefaultDocumentMergeStrategy extends DocumentMergeStrategy {
  def toRequirementAndEngine(r: Reportable, e: Option[Engine]): Option[RequirementAndEngine] =
    r match {
      case t: Test => None
      case r: RequirementAndHolder => Some(RequirementAndEngine(r, e))
      case _ => None
    }
  def merge(key: String, list: List[RequirementAndEngine]) = {
    list match {
      case Nil =>
        new SimpleRequirementAndHolder(None, None, None, None, Set(), List())
      case list =>
        makeFrom(key, list, List())
    }
  }
  def makeFrom(key: String, res: List[RequirementAndEngine], children: List[Reportable]): MergedReportable = {
    val titles = res.groupBy(_.reportable.title).collect {
      case (title, list) =>
        val descriptions = list.groupBy(_.reportable.description).collect {
          case (description, list) =>
            val newChildren = list.map((re) => re match {
              case (rh: RequirementAndHolder) =>
                RequirementAndEngine(SimpleRequirementAndHolder(rh, rh.children.filter(_.isInstanceOf[Test])), re.engine)
            })
            MergedDescription(description, newChildren)
        }.toList
        MergedTitle(title, descriptions)
    }.toList
    new MergedReportable(key, titles, children)
  }
}

object DocumentMergeStrategy {
  def default = new DefaultDocumentMergeStrategy
}

/** A reference is usually a link to a document. The 'ref' is a string of the form a.b.c..., where the fragments are string not containing a dot. The sort order is based on each fragment*/
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