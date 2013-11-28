package org.cddcore.engine

object Reportable {
  type ReportableList = List[Reportable]
  type ReportableSet = Set[Reportable]

  def allTests(list: List[Reportable]): List[Test] = list.flatMap(_ match { case t: Test => List(t); case rh: ReportableHolder => allTests(rh.children) })
}

trait HtmlDisplay {
  def htmlDisplay: String

}

case class UrlMap(val toUrl: Map[Reportable, String], val fromUrl: Map[String, List[Reportable]]) {
  def apply(r: Reportable): String = toUrl(r)
  def get(r: Reportable): Option[String] = toUrl.get(r)
  def apply(url: String) = fromUrl(url)
  def get(url: String) = fromUrl.get(url)
  def contains(r: Reportable) = toUrl.contains(r)
  def +(kv: (List[Reportable], String)) = UrlMap(toUrl + (kv._1.head -> kv._2), fromUrl + (kv._2 -> kv._1))
  def size = toUrl.size
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

  def all[R <: Reportable](rClass: Class[R]) = foldLeft[List[R]](List())((acc, r) => if (rClass.isAssignableFrom(r.getClass)) r.asInstanceOf[R] :: acc else acc)

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

object PathUtils {
  import Reportable._
  def findUseCase(path: ReportableList) = findUseCasePath(path).head.asInstanceOf[RequirementAndHolder]
  def findUseCasePath(path: ReportableList): ReportableList = path match {
    case (usecase: RequirementAndHolder) :: tail => path
    case h :: tail => findUseCasePath(tail)
    case _ => throw new IllegalArgumentException
  }

  def findEngineWithTests(path: ReportableList) = engineWithTestsPath(path).head.asInstanceOf[EngineBuiltFromTests[_]]
  def engineWithTestsPath(path: ReportableList): ReportableList = path match {
    case (engine: EngineBuiltFromTests[_]) :: tail => path
    case h :: tail => enginePath(tail)
    case _ => throw new IllegalArgumentException
  }
  def findEngine(path: ReportableList) = enginePath(path).head.asInstanceOf[Engine]
  def enginePath(path: ReportableList): ReportableList = path match {
    case (engine: Engine) :: tail => path
    case h :: tail => enginePath(tail)
    case _ => throw new IllegalArgumentException
  }
  def findProject(path: ReportableList) = projectPath(path).head.asInstanceOf[Project]
  def projectPath(path: ReportableList): ReportableList = path match {
    case (project: Project) :: tail => path
    case h :: tail => projectPath(tail)
    case _ => throw new IllegalArgumentException
  }
  def findReport(path: ReportableList) = reportPath(path).head.asInstanceOf[Report]
  def reportPath(path: ReportableList): ReportableList = path match {
    case (project: Report) :: tail => path
    case h :: tail => reportPath(tail)
    case _ => throw new IllegalArgumentException
  }

  def maxPriority(path: ReportableList) = path.foldLeft[Option[Int]](None)((acc, r: Reportable) =>
    (acc, r) match {
      case (None, (r: Requirement)) => r.priority
      case (Some(p), r: Requirement) if (r.priority.isDefined && r.priority.get > p) => Some(p);
      case (acc, _) => acc
    });

}

trait RequirementAndHolder extends ReportableHolder with Requirement

trait Requirement extends Reportable {
  def title: Option[String]
  def titleString = title.getOrElse("")
  def titleOrDescription(default: String): String = title.getOrElse(description.getOrElse(default))

  def description: Option[String]
  def priority: Option[Int]
  def references: List[Reference]
}

trait UseCase extends RequirementAndHolder {
  def optCode: Option[CodeHolder]
  def expected: Option[ROrException[_]]
}

trait Test extends Requirement {
  def optCode: Option[CodeHolder]
  def expected: Option[ROrException[_]]
  def because: Option[CodeHolder]
  def becauseString: String
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

case class Project(projectTitle: String, engines: ReportableHolder*) extends RequirementAndHolder {
  val title = Some(projectTitle)
  val children = engines.toList
  def description = None
  def priority = None
  def references = List()
}

object Engine {
  protected def addToList[R] = (acc: List[R], r: R) => r :: acc
  protected def addToSet[R] = (acc: Set[R], r: R) => acc + r
  val noInitialValue = () => throw new IllegalStateException
  def apply[P, R]() = new BuilderFactory1[P, R, R](None, noInitialValue).builder;
  def apply[P1, P2, R]() = new BuilderFactory2[P1, P2, R, R](None, noInitialValue).builder;
  def apply[P1, P2, P3, R]() = new BuilderFactory3[P1, P2, P3, R, R](None, noInitialValue).builder;

  def folding[P, R, FullR](foldFn: (FullR, R) => FullR, initialValue: => FullR) = new BuilderFactory1[P, R, FullR](Some(foldFn), () => initialValue).builder
  def folding[P1, P2, R, FullR](foldFn: (FullR, R) => FullR, initialValue: => FullR) = new BuilderFactory2[P1, P2, R, FullR](Some(foldFn), () => initialValue).builder
  def folding[P1, P2, P3, R, FullR](foldFn: (FullR, R) => FullR, initialValue: => FullR) = new BuilderFactory3[P1, P2, P3, R, FullR](Some(foldFn), () => initialValue).builder

  def foldList[P, R] = folding[P, R, List[R]](addToList[R], List[R]())
  def foldList[P1, P2, R] = folding[P1, P2, R, List[R]](addToList[R], List[R]())
  def foldList[P1, P2, P3, R] = folding[P1, P2, P3, R, List[R]](addToList[R], List[R]())

  def foldSet[P, R] = folding[P, R, Set[R]](addToSet[R], Set[R]())
  def foldSet[P1, P2, R] = folding[P1, P2, R, Set[R]](addToSet[R], Set[R]())
  def foldSet[P1, P2, P3, R] = folding[P1, P2, P3, R, Set[R]](addToSet[R], Set[R]())

  def state[S, P, R]() = new BuilderFactory2[S, P, (S, R), (S, R)](None, noInitialValue).builder;
  def state[S, P1, P2, R]() = new BuilderFactory3[S, P1, P2, (S, R), (S, R)](None, noInitialValue).builder;

  private var _traceBuilder: ThreadLocal[Option[TraceBuilder]] = new ThreadLocal[Option[TraceBuilder]] {
    override def initialValue = None
  }

  def call(e: Engine, params: List[Any]) = {
    _traceBuilder.get match {
      case Some(tb) => _traceBuilder.set(Some(tb.nest(e, params)))
      case None =>
    }
  }

  def endCall(conclusion: Conclusion, result: Any) {
    _traceBuilder.get match {
      case Some(tb) => _traceBuilder.set(Some(tb.finished(conclusion, result)))
      case None =>
    }
  }
  def failedCall(conclusion: Conclusion, exception: Throwable) {
    _traceBuilder.get match {
      case Some(tb) => _traceBuilder.set(Some(tb.failed(conclusion, exception)))
      case None =>
    }
  }
  def trace[T](x: => T): Tuple2[ROrException[T], List[TraceItem]] = {
    try {
      _traceBuilder.set(Some(new TraceBuilder(List())));
      try {
        val result = x
        (ROrException(result), _traceBuilder.get.get.items)
      } catch { case e: Throwable => (ROrException(e), _traceBuilder.get.get.items) }
    } finally
      _traceBuilder.set(None);
  }

  def testing = _testing
  private var _testing = false
  def test[T](x: () => T) = {
    _testing = true;
    try {
      x()
    } finally
      _testing = false
  }

}
case class ParamDetail(displayName: String, parser: (String) => _)

trait DecisionTreeFolder[Acc] {
  def apply(acc: Acc, c: Conclusion): Acc
  def apply(acc: Acc, d: Decision): Acc

}

trait Engine extends RequirementAndHolder {
  lazy val useCases = all(classOf[UseCase])
  def arity: Int
  def logger: TddLogger
  def decisionTreeNodes: Int

}
trait EngineBuiltFromTests[R] extends Engine {

  def root: Either[Conclusion, Decision]
  lazy val tests = all(classOf[Test])

  def evaluateBecauseForDecision(decision: Decision, params: List[Any]): Boolean
  def findConclusionFor(params: List[Any]): Conclusion
  def evaluateConclusion(params: List[Any], conclusion: Conclusion): R
  def evaluateConclusionNoException(params: List[Any], conclusion: Conclusion): ROrException[R]
  def apply(params: List[Any]): R = {
    Engine.call(this, params)
    val conclusion = findConclusionFor(params)
    val result = evaluateConclusion(params, conclusion)
    Engine.endCall(conclusion, result);
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
trait ChildEngine[R] extends EngineBuiltFromTests[R] {
}

trait EngineFull[R, FullR] extends Engine {
  import Reportable._
  def documents: List[Document]
  //  def root: Either[Conclusion, Decision]
  //  protected def toStringWith(path: ReportableList, root: Either[Conclusion, Decision], printer: IfThenPrinter): String
  def paramDetails: List[ParamDetail]
  lazy val childEngines: List[ChildEngine[R]] = all(classOf[ChildEngine[R]])
  def initialFoldValue: () => FullR
  def folder: Option[(FullR, R) => FullR]
  def applyParams(params: List[Any]): FullR =
    folder match {
      case Some(f) => childEngines.foldLeft[FullR](initialFoldValue())((acc, ce) => f(acc, ce(params)))
      case _ => throw new IllegalStateException
    }
  lazy val decisionTreeNodes = childEngines.foldLeft(0)(_ + _.decisionTreeNodes)
}

trait Engine1[P, R] extends Engine with Function1[P, R] {
  def arity = 1
}

trait Engine2[P1, P2, R] extends Engine with Function2[P1, P2, R] {
  def arity = 2
}

trait Engine3[P1, P2, P3, R] extends Engine with Function3[P1, P2, P3, R] {
  def arity = 3
}

trait ConclusionOrDecision extends Reportable {
  def allConclusion: Set[Conclusion]
  protected def allConclusion(either: Either[Conclusion, Decision]): Set[Conclusion] =
    either match {
      case Left(c) => c.allConclusion
      case Right(d) => d.allConclusion
    }
}

trait Decision extends ConclusionOrDecision {
  def because: List[CodeHolder]
  def yes: Either[Conclusion, Decision]
  def no: Either[Conclusion, Decision]
  def prettyString: String
  lazy val allYesConclusion = allConclusion(yes)
  lazy val allNoConclusion = allConclusion(no)
  lazy val allConclusion: Set[Conclusion] = allYesConclusion ++ allNoConclusion
}

trait Conclusion extends ConclusionOrDecision {
  def code: CodeHolder
  def scenarios: List[Test]
  val allConclusion = Set(this)
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