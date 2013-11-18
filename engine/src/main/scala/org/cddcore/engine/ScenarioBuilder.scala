package org.cddcore.engine

import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.concurrent.stm._
import java.text.MessageFormat
import scala.xml.NodeSeq
import java.lang.IllegalStateException

class NeedUseCaseException extends Exception
class NeedScenarioException extends Exception
class UndecidedException extends Exception
class CanOnlyAddDocumentToBuilderException extends Exception
class CanAddChildEngineAfterUseCaseOrScenarioException extends Exception
class CannotHaveChildEnginesWithoutFolderException extends Exception
class CannotFindDocumentException(msg: String) extends Exception(msg)
class ExceptionAddingScenario(msg: String, t: Throwable) extends EngineException(msg, t)
import Reportable._

/** R is the type returned by the child engines, or the engine if there are no child enginers. FullR is the result of the engine: which is the fold of the childEngine results if they exist */
trait EngineTypes[R, FUllR] {
  /** A is a function from the parameters of the engine, and the result to a boolean. It checks that some property is true */
  type A
  /** B is a function from the parameters of the engine to a boolean. It is effectively in calculating which scenario is to be used */
  type B
  /** RFn is a function from the parameters of the engine to a result R. It is used to calculate the result of the engine */
  type RFn
  type RFnMaker = (Either[Exception, R]) => RFn

  /** this is a function from the parameters to Unit e,g, (P1,P2,P3)=> Unit */
  type CfgFn;

  type RealScenarioBuilder
  /** In order to call an A in the building code, when we don't know anything about the arity, we create a closure holding the parameters and reusltand pass the A function to it */
  type AssertionClosure = (A) => Boolean
  /** In order to call a B in the building code, when we don't know anything about the arity, we create a closure holding the parameters and pass the B function to it */
  type BecauseClosure = (B) => Boolean
  /** In order to call a RFN in the building code, when we don't know anything about the arity, we create a closure holding the parameters and pass the B function to it */
  type ResultClosure = (RFn) => R
  type CfgClosure = (CfgFn) => Unit

  /** This is just a type synonym to save messy code */
  type Code = CodeFn[B, RFn, R]

  def makeClosureForAssertion(params: List[Any], r: ROrException[R]): AssertionClosure
  def makeClosureForBecause(params: List[Any]): BecauseClosure
  def makeClosureForResult(params: List[Any]): ResultClosure
  def makeClosureForCfg(params: List[Any]): CfgClosure

  /** turns parameters into fragment results. For example in XML in an engine 2 it might be (P1,P2)=>NodeSeq. I would love to know how to make this more type safe... */
  type FragFn
  /** turns the params into a Fragment. This can be modified by the fragment specifiers */
  type ParamsToFragmentFn
}
object ROrException {
  def apply[R]() = new ROrException[R](None, None)
  def apply[R](value: R) = new ROrException[R](Some(value), None)
  def apply[R](exception: Throwable) = new ROrException[R](None, Some(exception))

}
case class ROrException[R](value: Option[R], exception: Option[Throwable]) {
  if (value.isDefined && exception.isDefined) throw new IllegalStateException
  lazy val isDefined = value.isDefined || exception.isDefined
  lazy val isEmpty = !isDefined
  override def toString() = if (value.isDefined) "" + value.get else if (exception.isDefined) "throws " + exception.get.getClass.getSimpleName else "<N/A>"
  override def hashCode = value.hashCode + (if (exception.isDefined) exception.get.getClass.hashCode else 0)
  override def equals(obj: Any) = if (obj.getClass == classOf[ROrException[R]]) {
    (this, obj.asInstanceOf[ROrException[R]]) match {
      case (ROrException(Some(v1), None), ROrException(Some(v2), None)) => v1 == v2
      case (ROrException(Some(_), None), _) => false
      case (ROrException(None, Some(e1)), ROrException(None, Some(e2))) => e1.getClass == e2.getClass
      case (ROrException(None, Some(_)), _) => false
      case (ROrException(None, None), ROrException(None, None)) => true
    }
  } else false
}

trait EngineUniverse[R, FullR] extends EngineTypes[R, FullR] {

  object NodePath {
    implicit def toNode(p: NodePath) = p.parent
  }
  object PathPrinter {
    def apply(p: List[NodePath]): String = p.map((n) => Strings.oneLine(n.parent match {
      case Right(r) => r.scenarioThatCausedNode.because.description
      case Left(l) => "<Conclusion>"
    })).mkString("\n");
  }

  case class NodePath(parent: RorN, result: Boolean)

  object ExceptionScenarioPrinter {
    val fullScenario = false
    def apply(s: Scenario) = scenario2Str(s)
    def existingAndBeingAdded(existing: Scenario, s: Scenario) =
      s"Existing: ${existing.titleString}\nBeing Added: ${s.titleString}\nDetailed existing:\n${existing}\nDetailed of being Added:\n${s}"
    def full(s: Scenario) = s + "\nDetailed:\n  " + params(s)
    def params(s: Scenario) = s.paramPrinter(s.params)
    def scenario2Str(s: Scenario) =
      if (fullScenario)
        s.titleString
      else
        logger(s.params)

  }

  class ScenarioException(msg: String, val scenario: Scenario, cause: Throwable = null) extends EngineException(msg, cause)

  object NoExpectedException {
    def apply(scenario: Scenario, cause: Throwable = null) = new NoExpectedException(s"No expected in ${ExceptionScenarioPrinter.full(scenario)}", scenario, cause)
  }
  class NoExpectedException(msg: String, scenario: Scenario, cause: Throwable) extends ScenarioException(msg, scenario, cause)

  class EngineResultException(msg: String) extends EngineException(msg)
  class NoBecauseException(msg: String, scenario: Scenario) extends ScenarioException(msg, scenario)
  class ScenarioBecauseException(msg: String, scenario: Scenario) extends ScenarioException(msg, scenario)
  object ScenarioResultException {
    def adding(existing: Scenario, s: Scenario, actual: R) =
      new ScenarioResultException(s"\nActual Result: ${actual}\nExpected ${s.expected.getOrElse("<N/A>")}\n Scenario ${ExceptionScenarioPrinter(s)}\nExisting Scenario${ExceptionScenarioPrinter(s)}", s)
  }

  class ScenarioResultException(msg: String, scenario: Scenario) extends ScenarioException(msg, scenario)
  class AssertionException(msg: String, scenario: Scenario) extends ScenarioException(msg, scenario)

  object CannotDefineTitleTwiceException {
    def apply(original: String, beingAdded: String) = new CannotDefineTitleTwiceException(s"Original${original}\nBeing Added $beingAdded ", original, beingAdded);
  }
  class CannotDefineTitleTwiceException(msg: String, val original: String, val beingAdded: String) extends EngineException(msg, null)

  object CannotDefineDescriptionTwiceException {
    def apply(original: String, beingAdded: String) = new CannotDefineDescriptionTwiceException(s"Original${original}\nBeing Added $beingAdded ", original, beingAdded);
  }
  class CannotDefineDescriptionTwiceException(msg: String, val original: String, val beingAdded: String) extends EngineException(msg, null)

  object CannotDefineExpectedTwiceException {
    def apply(original: ROrException[R], beingAdded: ROrException[R]) = new CannotDefineExpectedTwiceException(s"Original${original}\nBeing Added $beingAdded ", original, beingAdded);
  }
  class CannotDefineExpectedTwiceException(msg: String, val original: ROrException[R], val beingAdded: ROrException[R]) extends EngineException(msg, null)

  class CannotDefineCodeTwiceException(msg: String, val original: Code, val beingAdded: Code) extends EngineException(msg, null)
  object CannotDefineCodeTwiceException {
    def apply(original: Code, beingAdded: Code) = new CannotDefineCodeTwiceException(s"Original Code:\n${original}\nBeingAdded\n${beingAdded}", original, beingAdded);
  }
  class CannotDefineBecauseTwiceException(msg: String, val original: Because[B], val beingAdded: Because[B]) extends EngineException(msg, null)
  object CannotDefineBecauseTwiceException {
    def apply(original: Because[B], beingAdded: Because[B]) = new CannotDefineBecauseTwiceException(s"Original Because:\n${original}\nBeingAdded\n${beingAdded}", original, beingAdded);
  }

  object ScenarioConflictingWithDefaultException {
    def apply(actual: ROrException[R], s: Scenario) =
      new ScenarioConflictingWithDefaultException(s"\nActual Result: ${actual}\nExpected ${s.expected.getOrElse("<N/A>")}\n Scenario ${ExceptionScenarioPrinter.full(s)}", actual, s)
  }
  class ScenarioConflictingWithDefaultException(msg: String, val actual: ROrException[R], scenario: Scenario) extends ScenarioException(msg, scenario)

  object ScenarioConflictingWithoutBecauseException {
    def apply(expected: ROrException[R], actual: ROrException[R], parents: List[NodePath], beingAdded: Scenario) = {
      val pathString = PathPrinter(parents)
      parents match {
        case NodePath(Left(l: CodeAndScenarios), _) :: Nil => throw ScenarioConflictingWithDefaultException(actual, beingAdded);
        case NodePath(Left(l: CodeAndScenarios), _) :: tail =>
          l.addedBy match {
            case Some(existing) =>
              new ScenarioConflictingWithoutBecauseException(s"\nCame to wrong conclusion: ${actual}\nInstead of ${expected}\nPath\n$pathString\n${ExceptionScenarioPrinter.existingAndBeingAdded(existing, beingAdded)}", existing, beingAdded)
            case None => throw ScenarioConflictingWithDefaultException(actual, beingAdded);
          }
        case _ => throw new IllegalStateException;
      }
    }
  }

  class ScenarioConflictingWithoutBecauseException(msg: String, scenario: Scenario, beingAdded: Scenario, cause: Throwable = null) extends ScenarioConflictException(msg, scenario, beingAdded, cause)

  object ScenarioConflictException {
    def apply(existing: Scenario, beingAdded: Scenario, cause: Throwable = null) =
      new ScenarioConflictException(s"Cannot differentiate based on:\n ${beingAdded.becauseString}" +
        s"\n${ExceptionScenarioPrinter.existingAndBeingAdded(existing, beingAdded)}", existing, beingAdded, cause)
  }
  class ScenarioConflictException(msg: String, scenario: Scenario, val beingAdded: Scenario, cause: Throwable = null) extends ScenarioException(msg, scenario, cause)
  class MultipleExceptions(msg: String, val scenarioExceptionMap: ScenarioExceptionMap) extends EngineException(msg, scenarioExceptionMap.first.get)

  object WrongExceptionThrownException {
    def apply(scenario: Scenario, expected: Class[_ <: Throwable], actual: Throwable) = new WrongExceptionThrownException(s"Expected ${expected.getClass.getSimpleName}", scenario, expected, actual)
  }
  class WrongExceptionThrownException(msg: String, scenario: Scenario, val expected: Class[_ <: Throwable], actual: Throwable) extends ScenarioException(msg, scenario, actual)
  object NoExceptionThrownException {
    def apply(scenario: Scenario, expected: Class[_ <: Throwable], actual: Any) = new NoExceptionThrownException(s"Expected ${expected.getClass.getSimpleName}", scenario, expected, actual)
  }
  class NoExceptionThrownException(msg: String, scenario: Scenario, val expected: Class[_ <: Throwable], actual: Any) extends ScenarioException(msg, scenario)

  object AssertionDoesntMatchBecauseException {
    def apply(existing: Scenario, beingAdded: Scenario) = new AssertionDoesntMatchBecauseException(ExceptionScenarioPrinter.existingAndBeingAdded(existing, beingAdded), existing, beingAdded)
  }
  class AssertionDoesntMatchBecauseException(msg: String, scenario: Scenario, beingAdded: Scenario) extends ScenarioConflictException(msg, scenario, beingAdded)

  object ExceptionWithoutCodeException {
    def apply(s: Scenario) = new ExceptionWithoutCodeException(s.toString, s)
  }
  class ExceptionWithoutCodeException(msg: String, scenario: Scenario) extends ScenarioException(msg, scenario)

  def rfnMaker: (Either[() => Exception, R]) => RFn
  def logger: TddLogger

  type RorN = Either[CodeAndScenarios, EngineNode]
  type RealScenarioBuilder <: ScenarioBuilder

  def builder: RealScenarioBuilder

  object CodeAndScenarios {
    implicit def delegate_to[B, RFn, R](c: CodeAndScenarios) = c.code
  }

  case class CodeAndScenarios(val code: Code, val scenarios: List[Scenario] = List(), default: Boolean = false) extends Conclusion {

    def addedBy = scenarios match {
      case Nil => None
      case _ => Some(scenarios.last)
    }
    override def toString() = {
      val defaultString = if (default) "default, " else ""
      getClass.getSimpleName + "(" + defaultString + code + ":" + scenarios.map(_.description).mkString(",") + ")";
    }
  }

  case class EngineNode(val because: List[Because[B]], inputs: List[Any], yes: RorN, no: RorN, scenarioThatCausedNode: Scenario) extends Decision {
    def allScenarios = scenarios(Right(this))
    def becauseString = because.map(_.description).mkString(" or ")
    def prettyString = because.map(_.pretty).mkString(" or ")
    private def scenarios(rOrN: RorN): List[Scenario] = {
      rOrN match {
        case Left(cd) => cd.scenarios
        case Right(n) => scenarios(n.yes) ++ scenarios(n.no)
      }
    }
    def evaluateBecause(fn: BecauseClosure): Boolean = {
      for (b <- because)
        if (fn(b.because))
          return true
      false

    }
    override def toString = getClass().getSimpleName() + "(" + becauseString + " => " + yes.toString() + " =/> " + no.toString() + " / " + scenarioThatCausedNode.description + ")";
  }

  trait BuilderNode extends Requirement {
    def expected: Option[ROrException[R]]
    def optCode: Option[Code]
  }

  case class Scenario(
    title: Option[String],
    description: Option[String],
    params: List[Any],
    paramPrinter: LoggerDisplayProcessor,
    expected: Option[ROrException[R]] = None,
    optCode: Option[Code] = None,
    because: Option[Because[B]] = None,
    assertions: List[Assertion[A]] = List(),
    configuration: Option[CfgFn] = None,
    priority: Option[Int] = None,
    references: List[Reference] = List()) extends BuilderNode with Requirement with Test {

    def configure = if (configuration.isDefined) makeClosureForCfg(params)(configuration.get)
    lazy val actualCode: CodeFn[B, RFn, R] = optCode.getOrElse({
      //    println("Expected: " + expected)
      //    println("Constraint: " + this)
      expected match {
        case Some(ROrException(Some(value), None)) => new CodeFn(rfnMaker(Right(value)), value.toString);
        case Some(ROrException(None, Some(_))) => throw new IllegalStateException("Internal Error: Have no code for an exception");
        case _ => new CodeFn(rfnMaker(Left(() => new IllegalStateException("Do not have code or expected  for this scenario: " + ExceptionScenarioPrinter(Scenario.this)))), "No expected or Code")
      }
    })
    def becauseString = because match { case Some(b) => b.description; case _ => "" }
    override def titleString = title.getOrElse(paramString)
    lazy val paramString = params.map(paramPrinter).mkString(",")
    override def toString =
      s"Scenario(${title.getOrElse("")}, ${paramString}, because=${becauseString}, expected=${logger(expected.getOrElse("<N/A>"))})"
  }

  case class ScenarioExceptionMap(map: Map[Scenario, Throwable] = Map(), first: Option[Throwable] = None) {
    def size = map.size
    def values = map.values
    def +(x: (Scenario, Throwable)) = ScenarioExceptionMap(map + x, Some(first.getOrElse(x._2)))
    def apply(s: Scenario) = map(s)
    def contains(s: Scenario) = map.contains(s)
  }

  case class UseCaseDescription(title: Option[String] = None, description: Option[String] = None, children: List[Reportable] = List(), expected: Option[ROrException[R]] = None, optCode: Option[Code] = None, priority: Option[Int] = None, references: List[Reference] = List()) extends BuilderNode with UseCase {
    override val templateName = "UseCase"
    override def toString = "UseCase(" + titleString + " children=" + children.mkString(",") + ")"
  }
  case class ChildEngineDescription(title: Option[String] = None, description: Option[String] = None, children: List[Reportable] = List(), expected: Option[ROrException[R]] = None, optCode: Option[Code] = None, priority: Option[Int] = None, references: List[Reference] = List()) extends BuilderNode with ChildEngine {
    override def toString = "ChildEngine(" + titleString + " children=" + children.mkString(",") + ")"
  }

  class ChildEngineImpl(val desc: ChildEngineDescription) extends ChildEngine with BuildEngine {
    def priority = desc.priority
    def title = desc.title
    def description = desc.description
    def children = desc.children
    def references = desc.references
    def optCode = desc.optCode
    override lazy val scenarios = all(classOf[Scenario])
  }

  /** This holds the data that the scenario builder is building. It is a separate class mainly to allow code insight to be nicer for the scenarioBuilder. For example we want the description method to add a description on the builder, while we want to be able to access the description from other classes. Code insight is nicer with just one description*/
  case class ScenarioBuilderData(
    depth: Int = 0,
    title: Option[String] = None,
    description: Option[String] = None,
    children: ReportableList = List(),
    folder: (List[R], R) => FullR = ((acc, r) => if (acc.size > 1) throw new CannotHaveChildEnginesWithoutFolderException else r.asInstanceOf[FullR]),
    expected: Option[ROrException[R]] = None,
    optCode: Option[Code] = None,
    priority: Option[Int] = None,
    documents: List[Document] = List(),
    paramDetails: List[ParamDetail] = List(),
    references: List[Reference] = List()) extends BuilderNode with RequirementAndHolder {

    def firstExpected(path: ReportableList): Option[ROrException[R]] = path.foldLeft[Option[ROrException[R]]](None)((acc, r: Reportable) =>
      (acc, r) match {
        case (None, (s: Scenario)) => s.expected
        case (None, (u: UseCaseDescription)) => u.expected
        case (None, (sb: ScenarioBuilderData)) => sb.expected
        case (acc, _) => acc
      });

    protected def modifyChildForBuild(path: ReportableList): Reportable = path.head match {
      case ce: ChildEngineDescription => new ChildEngineImpl(ce.copy(children = modifyChildrenForBuild(ce.children, path)))
      case u: UseCaseDescription => u.copy(children = modifyChildrenForBuild(u.children, path))
      case s: Scenario => s.copy(priority = PathUtils.maxPriority(path), expected = firstExpected(path))
    }
    protected def modifyChildrenForBuild(children: List[Reportable], path: ReportableList): ReportableList = children.map((r) => modifyChildForBuild(r :: path))

    lazy val childrenModifiedForBuild = modifyChildrenForBuild(children, List(this));

  }
  def validateBecause(s: Scenario) = {
    s.configure
    s.because match {
      case Some(b) =>
        if (!makeClosureForBecause(s.params).apply(b.because))
          throw new ScenarioBecauseException(s.becauseString + " is not true for " + ExceptionScenarioPrinter.full(s) + "\n", s);
      case None =>
    }
  }

  trait ScenarioBuilder {

    def builderData: ScenarioBuilderData

    protected def set[X](x: X, bFn: (ScenarioBuilder, X) => ScenarioBuilder, ceFn: (ChildEngineDescription, X) => ChildEngineDescription, uFn: (UseCaseDescription, X) => UseCaseDescription, sFn: (Scenario, X) => Scenario, checkFn: (BuilderNode, X) => Unit = (b: BuilderNode, x: X) => {}) =
      setWithDepth(0, this, x, bFn, ceFn, uFn, sFn, checkFn).asInstanceOf[RealScenarioBuilder]

    protected def setWithDepth[Node, X](
      depth: Int,
      node: Node,
      x: X,
      bFn: (ScenarioBuilder, X) => ScenarioBuilder,
      ceFn: (ChildEngineDescription, X) => ChildEngineDescription,
      uFn: (UseCaseDescription, X) => UseCaseDescription,
      sFn: (Scenario, X) => Scenario,
      checkFn: (BuilderNode, X) => Unit = (b: BuilderNode, x: X) => {}): Node =
      {
        def descend(children: List[Reportable]): List[Reportable] =
          children match {
            case (childEngine: ChildEngineDescription) :: tail => setWithDepth(depth + 1, childEngine, x, bFn, ceFn, uFn, sFn, checkFn) :: tail
            case (useCase: UseCase) :: tail => setWithDepth(depth + 1, useCase, x, bFn, ceFn, uFn, sFn, checkFn) :: tail
            case (scenario: Scenario) :: tail => setWithDepth(depth + 1, scenario, x, bFn, ceFn, uFn, sFn, checkFn) :: tail
          }

        if (depth == builderData.depth) {
          node match {
            case builder: ScenarioBuilder => { checkFn(builder.builderData, x); bFn(builder, x).asInstanceOf[Node] }
            case childEngine: ChildEngineDescription => { checkFn(childEngine, x); ceFn(childEngine, x).asInstanceOf[Node] }
            case useCase: UseCaseDescription => { checkFn(useCase, x); uFn(useCase, x).asInstanceOf[Node] }
            case scenario: Scenario => { checkFn(scenario, x); sFn(scenario, x).asInstanceOf[Node] }
          }
        } else
          node match {
            case builder: ScenarioBuilder => builder.copy(children = descend(builder.builderData.children)).asInstanceOf[Node]
            case childEngine: ChildEngineDescription => childEngine.copy(children = descend(childEngine.children)).asInstanceOf[Node]
            case useCase: UseCaseDescription => useCase.copy(children = descend(useCase.children)).asInstanceOf[Node]
          }
      }

    protected def getParent: Option[Object] = if (builderData.depth == 0) None else Some(getParentAt(1, this))
    def buildTarget: Object = if (builderData.depth == 0) this else getBuildTarget(0, this)

    protected def getBuildTarget(depth: Int, node: Object): Object = {
      def getBuildTargetFromChildren(children: List[Reportable]): Object = if (children.isEmpty) throw new IllegalStateException else getBuildTarget(depth + 1, children.head)
      if (depth == builderData.depth)
        node
      else node match {
        case b: ScenarioBuilder => getBuildTargetFromChildren(b.builderData.children)
        case r: ReportableHolder => getBuildTargetFromChildren(r.children)
      }

    }

    protected def getParentAt(depth: Int, parent: Object): Object = {
      def getParentFromChildren(children: List[Reportable]) = if (children.isEmpty) throw new IllegalStateException else
        getParentAt(depth + 1, children.head)
      if (depth == builderData.depth)
        parent
      else parent match {
        case b: ScenarioBuilder => getParentFromChildren(b.builderData.children)
        case r: ReportableHolder => getParentFromChildren(r.children)
      }
    }
    /** You should probably not call this explicitly. This makes a copy of the builder with new data. The builder is an immutable object, so this is how data is actually 'changed' when you call methods like description */
    def copy(builderData: ScenarioBuilderData): RealScenarioBuilder

    protected def copy(
      depth: Int = builderData.depth,
      title: Option[String] = builderData.title,
      description: Option[String] = builderData.description,
      children: ReportableList = builderData.children,
      folder: (List[R], R) => FullR = builderData.folder,
      optCode: Option[Code] = builderData.optCode,
      expected: Option[ROrException[R]] = builderData.expected,
      priority: Option[Int] = builderData.priority,
      references: List[Reference] = builderData.references,
      documents: List[Document] = builderData.documents,
      paramDetails: List[ParamDetail] = builderData.paramDetails): RealScenarioBuilder =
      copy(ScenarioBuilderData(depth, title, description, children, folder, expected, optCode, priority, documents, paramDetails, references))

    protected def thisAsBuilder: RealScenarioBuilder

    /** This adds the documents you specify to the documents that the builder 'knows about'. Currently the documents are only used by references.*/
    def document(documents: Document*) = {
      set[List[Document]](documents.toList ++ this.builderData.documents,
        (b, d) =>
          b.copy(documents = d),
        (ce, d) => throw new CanOnlyAddDocumentToBuilderException,
        (u, d) => throw new CanOnlyAddDocumentToBuilderException,
        (s, d) => throw new CanOnlyAddDocumentToBuilderException,
        (n, d) => {});

    }
    /** Set the title of the 'last thing to be mentioned' i.e. the builder/usecase/scenario. Throws CannotDefineTitleTwiceException if title already set. The title is used when creating reports */
    def title(title: String): RealScenarioBuilder =
      set[Option[String]](Some(title),
        (b, title) => b.copy(title = title),
        (ce, title) => ce.copy(title = title),
        (u, title) => u.copy(title = title),
        (s, title) => s.copy(title = title),
        (n, title) => if (n.title.isDefined) throw CannotDefineTitleTwiceException(n.title.get, title.get))

    /** Set the description of the 'last thing to be mentioned' i.e. the builder/usecase/scenario. Throws CannotDefineDescriptionTwiceException if title already set. The description is used when creating reports */
    def description(description: String): RealScenarioBuilder =
      set[Option[String]](Some(description),
        (b, description) => b.copy(description = description),
        (ce, description) => ce.copy(description = description),
        (u, description) => u.copy(description = description),
        (s, description) => s.copy(description = description),
        (n, description) => if (n.description.isDefined) throw CannotDefineDescriptionTwiceException(n.description.get, description.get))

    /** This is only used when using the 'live' website option. It turns a string into a parameter, and gives the parameter a 'nice' name on the website */
    def param(parser: (String) => _, name: String = s"Param${builderData.paramDetails.size}") = copy(paramDetails = ParamDetail(name, parser) :: builderData.paramDetails)

    /**
     * Set the 'expected' of the 'last thing to be mentioned' to be an exception. i.e. the builder/usecase/scenario. Throws CannotDefineExpectedTwiceException if expected already set.
     *
     *  Note that the engine will currently only check the type of the message, not the message itself, and that you must have a 'code' to throw the exception
     */
    def expectException[E <: Throwable](e: E, comment: String = "") =
      set[Option[ROrException[R]]](Some(ROrException[R](e)),
        (b, newE) => b.copy(expected = newE),
        (ce, newE) => ce.copy(expected = newE),
        (u, newE) => u.copy(expected = newE),
        (s, newE) => s.copy(expected = newE),
        (n, newE) => if (n.expected.isDefined) throw CannotDefineExpectedTwiceException(n.expected.get, newE.get))

    /** Set the 'expected' of the 'last thing to be mentioned' to be this value. i.e. the builder/usecase/scenario. Throws CannotDefineExpectedTwiceException if expected already set. If you haven't set a code, then this will also act as the code*/
    def expected(e: R): RealScenarioBuilder =
      set[Option[ROrException[R]]](Some(ROrException[R](e)),
        (b, newExpected) => b.copy(expected = newExpected),
        (ce, newExpected) => ce.copy(expected = newExpected),
        (u, newExpected) => u.copy(expected = newExpected),
        (s, newExpected) => s.copy(expected = newExpected),
        (n, newExpected) => if (n.expected.isDefined) throw CannotDefineExpectedTwiceException(n.expected.get, newExpected.get))

    /**
     * This is optional, as it defaults to returning 'expected'. The code is the code that you would use to generate the expected.
     *
     *  The TennisScorer is a good example of where this is needed :
     * <pre>     scenario(2, 3).expected("thirty, forty").code((l: Int, r: Int) => s"${lookup(l)}, ${lookup(r)}").</pre>
     *
     *  Although in this case 'thirty forty' is the expected, we want to calculate the value so that it can be generalised for other inputs
     *
     */
    def code(c: Code, comment: String = "") =
      set[Option[Code]](Some(c),
        (b, newCode) => b.copy(optCode = newCode),
        (ce, newCode) => ce.copy(optCode = newCode),
        (u, newCode) => u.copy(optCode = newCode),
        (s, newCode) => s.copy(optCode = newCode),
        (n, newCode) => if (n.optCode.isDefined) throw CannotDefineCodeTwiceException(n.optCode.get, newCode.get))

    /** Set the 'priority' of the 'last thing to be mentioned' to be this value. i.e. the builder/usecase/scenario. This determines the 'rule ordering' of the scenarios. If you don't like the produced decision tree, you can increase /decrease the priority of a scenario with the because clause you want to move     */
    def priority(priority: Int): RealScenarioBuilder =
      set[Int](
        priority,
        (b, priority) => b.copy(priority = Some(priority)),
        (ce, priority) => ce.copy(priority = Some(priority)),
        (u, priority) => u.copy(priority = Some(priority)),
        (s, priority) => s.copy(priority = Some(priority)))

    protected def findDocument(documentName: String) = builderData.documents.find((d) => d.name == Some(documentName)) match {
      case Some(d) => d
      case None => throw new CannotFindDocumentException(documentName);
    }

    /** Adds a reference to the 'last thing to be mentioned'. References are currently just used in the reports. If the documentName isn't in the list of doucments, this will throw a  CannotFindDocumentException*/
    def reference(ref: String, documentName: String): RealScenarioBuilder = reference(ref, findDocument(documentName))

    /** Adds a reference to the 'last thing to be mentioned'. References are currently just used in the reports. */
    def reference(ref: String, document: Document): RealScenarioBuilder = reference(ref, Some(document))

    /** Adds a reference to the 'last thing to be mentioned'. This is just the 'ref' string, without a document */
    def reference(ref: String, document: Option[Document] = None): RealScenarioBuilder =
      set[Reference](
        Reference(ref, document),
        (b, r) => b.copy(references = r :: b.builderData.references),
        (ce, r) => ce.copy(references = r :: ce.references),
        (u, r) => u.copy(references = r :: u.references),
        (s, r) => s.copy(references = r :: s.references))

    /**
     * This method is used if you are action on mutable parameters. The cgnFn is called to setup the parameters prior to them being used in the because clause
     *
     *   The cfgFn is only used while building the engine. An example use would be when the parameter is a mutable object like a Swing JTextArea. The cfgFn would setup the textarea to have the correct values
     */
    def configuration[K](cfg: CfgFn) = set[CfgFn](cfg,
      (_, _) => throw new NeedScenarioException,
      (_, _) => throw new NeedScenarioException,
      (_, _) => throw new NeedScenarioException,
      (s, c) => s.copy(configuration = Some(c)))

    /** An assertion is an extra test to be run. The assertions must be true for the scenario when the engine has been built. The function you pass in to it takes the parameters and the result of the engine, and returns a boolean*/
    def assertion(a: Assertion[A], comment: String = "") =
      set[Assertion[A]](a,
        (_, _) => throw new NeedScenarioException,
        (_, _) => throw new NeedScenarioException,
        (_, _) => throw new NeedScenarioException,
        (s, r) => s.copy(assertions = a.copy(comment = comment) :: s.assertions))

    /** Set the 'because' of the 'last thing to be mentioned' to be this value. i.e. the builder/usecase/scenario. Throws CannotDefineBecauseTwiceException if expected already set. If you haven't set a code, then this will also act as the code*/
    def because(b: Because[B], comment: String = "") =
      set[Because[B]](b.copy(comment = comment),
        (_, _) => throw new NeedScenarioException,
        (_, _) => throw new NeedScenarioException,
        (_, _) => throw new NeedScenarioException,
        (s, b) => {
          if (s.because.isDefined)
            throw CannotDefineBecauseTwiceException(s.because.get, b)
          validateBecause(s);
          s.copy(because = Some(b))
        })

    def childEngine(engineTitle: String) = {
      for (c <- builderData.children)
        if (!c.isInstanceOf[ChildEngineDescription])
          throw new CanAddChildEngineAfterUseCaseOrScenarioException

      copy(builderData.copy(depth = 1, children = new ChildEngineDescription(title = Some(engineTitle)) :: builderData.children))
    }

    protected def withUseCase(useCaseTitle: String, useCaseDescription: Option[String]) = {
      def newUseCase(parentDepth: Int) = {
        val childDepth = parentDepth + 1
        val useCase = UseCaseDescription(Some(useCaseTitle), useCaseDescription)
        val atParentDepth = copy(depth = parentDepth)
        val result = atParentDepth.set[UseCaseDescription](useCase,
          (b, u) =>
            b.copy(children = u :: b.builderData.children),
          (ce, u) =>
            ce.copy(children = u :: ce.children),
          (uc, u) =>
            u.copy(children = u :: uc.children),
          (_, _) => throw new IllegalStateException).copy(depth = childDepth)
        result
      }
      val p = getParent
      val me = buildTarget
      val depth = builderData.depth
      (p, me) match {
        case (None, sb: ScenarioBuilder) => newUseCase(depth)
        case (Some(sb: ScenarioBuilder), ce: ChildEngineDescription) => newUseCase(depth)
        case (Some(sb: ScenarioBuilder), u: UseCase) => newUseCase(depth - 1)
        case (Some(ce: ChildEngineDescription), u: UseCase) => newUseCase(depth - 1)
        case (Some(sb: ScenarioBuilder), s: Scenario) => newUseCase(depth - 1)
        case (Some(ce: ChildEngineDescription), s: Scenario) => newUseCase(depth - 1)
        case (Some(uc: UseCase), s: Scenario) => newUseCase(depth - 2)
      }
      //      copy(children = UseCase(title = Some(useCaseTitle), description = useCaseDescription) :: builderData.children);
    }

    /** Starts the definition of a new case. A use case can be thought of as a list of scenarios */
    def useCase(useCaseTitle: String) = withUseCase(useCaseTitle, None)
    /** Starts the definition of a new case. A use case can be thought of as a list of scenarios */
    def useCase(useCaseTitle: String, useCaseDescription: String) = withUseCase(useCaseTitle, Some(useCaseDescription))

    protected def newScenario(scenarioTitle: String, scenarioDescription: String, params: List[Any]): RealScenarioBuilder = {
      def newScenario(parentsDepth: Int) = {
        val childDepth = parentsDepth + 1
        val titleString = if (scenarioTitle == null) None else Some(scenarioTitle);
        val descriptionString = if (scenarioDescription == null) None else Some(scenarioDescription);
        val scenario = new Scenario(titleString, descriptionString, params, logger)
        copy(depth = parentsDepth).set[Scenario](scenario,
          (b, s) => b.copy(children = s :: b.builderData.children),
          (ce, s) => ce.copy(children = s :: ce.children),
          (u, s) => u.copy(children = s :: u.children),
          (_, _) => throw new IllegalStateException).copy(depth = childDepth)
      }
      val depth = builderData.depth
      val p = getParent
      val m = buildTarget
      (p, m) match {
        case (None, sb: ScenarioBuilder) =>
          newScenario(0)
        case (sb: ScenarioBuilder, ce: ChildEngine) =>
          newScenario(1)
        case (Some(parent: ScenarioBuilder), u: UseCase) =>
          newScenario(1)
        case (Some(parent: ScenarioBuilder), s: Scenario) =>
          newScenario(0)
        case (Some(parent: ScenarioBuilder), ce: ChildEngineDescription) =>
          newScenario(1)
        case (Some(parent: ChildEngineDescription), s: Scenario) =>
          newScenario(1)
        case (Some(parent: ChildEngineDescription), u: UseCase) =>
          newScenario(2)
        case (Some(parent: UseCase), s: Scenario) =>
          newScenario(depth - 1)
        case _ =>
          throw new RuntimeException(s"Don't know how to match $p, $m")
      }
    }

    //      builderData.children match {
    //      case (h: UseCase) :: t => {
    //        val titleString = if (scenarioTitle == null) None else Some(scenarioTitle);
    //        val descriptionString = if (scenarioDescription == null) None else Some(scenarioDescription);
    //        copy(children = h.copy(children = Scenario(titleString, descriptionString, params, logger) :: h.scenarios) :: t)
    //      }
    //      case _ => throw new NeedUseCaseException
    //    }
  }

  trait EvaluateEngine {

    def evaluate(fn: BecauseClosure, n: RorN, log: Boolean = true): CodeAndScenarios = {
      val result = n match {
        case Left(r) =>
          val result = r
          result
        case Right(n) => evaluate(fn, n, log)
      }
      result
    }

    protected def findConclusionFor(fn: BecauseClosure, n: RorN): CodeAndScenarios = {
      n match {
        case Left(c: Conclusion) => c;
        case Right(r) =>
          val condition = r.evaluateBecause(fn)
          condition match {
            case false => findConclusionFor(fn, r.no);
            case true => findConclusionFor(fn, r.yes);
          }
      }
    }

    private def evaluate(fn: BecauseClosure, n: EngineNode, log: Boolean): CodeAndScenarios = {
      val condition = n.evaluateBecause(fn)
      if (log)
        logger.evaluating(n.because, condition)
      condition match {
        case false => evaluate(fn, n.no, log);
        case true => evaluate(fn, n.yes, log);
      }
    }
  }
  trait EngineToString[R] extends SimpleEngine[R] {

    def root: Either[Conclusion, Decision]

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

    def increasingScenariosList(cs: List[Scenario]): List[List[Scenario]] =
      cs.foldLeft(List[List[Scenario]]())((a, c) => (a match {
        case (h :: t) => (c :: a.head) :: a;
        case _ => List(List(c))
      }))

    def titleString: String

    def toStringWith(path: List[Reportable], root: Either[Conclusion, Decision], printer: IfThenPrinter): String =
      printer.start(path, this) + toStringPrimWith(path, root, printer) + printer.end

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

  trait BuildEngine extends EvaluateEngine with EngineToString[R] {
    import org.cddcore.engine.Engine._
    def optCode: Option[Code]
    def scenarios: List[Scenario]
    val defaultRoot: RorN = optCode match {
      case Some(code) => Left(new CodeAndScenarios(code, List(), true))
      case _ => Left(new CodeAndScenarios(rfnMaker(Left(() =>
        new UndecidedException)), List(), true))
    }
    private val rootAndExceptionMap = buildRoot(defaultRoot, scenarios)

    val root: RorN = rootAndExceptionMap._1

    val scenarioExceptionMap: ScenarioExceptionMap = rootAndExceptionMap._2

    lazy val decisionTreeNodes = countDecisionTreeNodes(root, 0)

    def countDecisionTreeNodes(n: RorN, sum: Int): Int =
      n match {
        case Left(c) => sum + 1
        case Right(n) => sum + countDecisionTreeNodes(n.yes, 0) + countDecisionTreeNodes(n.no, 0) + 1
      }

    def buildRoot(root: RorN, scenarios: List[Scenario]): (RorN, ScenarioExceptionMap) = {
      scenarios match {
        case s :: rest =>
          val newRootAndExceptionMap = buildFromScenarios(root, scenarios, ScenarioExceptionMap());
          val (newRoot, seMap) = newRootAndExceptionMap
          if (!testing) {
            seMap.size match {
              case 0 => ;
              case 1 => throw seMap.values.head
              case _ =>
                //                for (e <- seMap.values)
                //                  e.printStackTrace()
                throw new MultipleExceptions(s"Could not build Engine $seMap", seMap)
            }
          }
          newRootAndExceptionMap

        case _ => (root, ScenarioExceptionMap())
      }

    }

    protected def checkExpectedMatchesAction(root: RorN, s: Scenario) {
      s.configure
      val bec = makeClosureForBecause(s.params)
      val rFn: RFn = evaluate(bec, root, false).code.rfn
      val actualFromScenario: ROrException[R] = safeCall(makeClosureForResult(s.params), rFn)
      if (s.expected.isEmpty)
        throw NoExpectedException(s)
      if (actualFromScenario != s.expected)
        throw new ScenarioResultException("Wrong result for " + s.actualCode.description + " for " + ExceptionScenarioPrinter(s) + "\nActual: " + actualFromScenario + "\nExpected: " + s.expected.getOrElse("<N/A>") + "\nRoot:\n" + toString("", root), s);
    }

    protected def validateScenario(root: RorN, s: Scenario) {
      s.configure
      val bec = makeClosureForBecause(s.params)
      val rFn: RFn = evaluate(bec, root, false).code.rfn
      val actualFromScenario: ROrException[R] = safeCall(makeClosureForResult(s.params), rFn)
      if (s.expected.isEmpty)
        throw NoExpectedException(s)
      if (Some(actualFromScenario) != s.expected)
        throw new ScenarioResultException("Wrong result for " + s.actualCode.description + " for " + ExceptionScenarioPrinter(s) + "\nActual: " + actualFromScenario + "\nExpected: " + s.expected.getOrElse("<N/A>") + "\nRoot:\n" + toString("", root), s);
      val assertionClosure = makeClosureForAssertion(s.params, actualFromScenario);
      for (a <- s.assertions) {
        val result = assertionClosure(a.assertion)
        if (!result)
          throw new AssertionException("\nAssertion " + a.description + " failed.\nParams are " + s.params + "\nResult was " + actualFromScenario, s)
      }

    }

    def buildFromScenarios(root: RorN, cs: List[Scenario], seMap: ScenarioExceptionMap): (RorN, ScenarioExceptionMap) = {
      val sorted = cs.sortBy(-_.priority.getOrElse(0))
      sorted match {
        case c :: tail =>
          try {
            validateBecause(c);
            c.expected match {
              case Some(ROrException(None, Some(e))) => if (!c.optCode.isDefined) throw ExceptionWithoutCodeException(c)
              case _ => ;
            }
            val newRoot = withScenario(root, c)
            //            val newRoot = withScenario(List(), root, c, true)
            validateScenario(newRoot, c);
            buildFromScenarios(newRoot, tail, seMap)
          } catch {
            case e: ThreadDeath =>
              throw e
            case e: Throwable =>
              e.printStackTrace()
              buildFromScenarios(root, tail, seMap + (c -> e))
          }

        case _ => (root, seMap);
      }
    }

    def validateScenarios(root: RorN, scenarios: List[Scenario]) {
      import org.cddcore.engine.Engine._
      if (root == null)
        if (testing)
          return
        else
          throw new NullPointerException("Cannot validate scenario as root doesn't exist")
      for (s <- scenarios) {
        s.configure
        val bc = makeClosureForBecause(s.params)
        val fnr = makeClosureForResult(s.params)
        val conclusionNode = evaluate(bc, root, false);
        if (!conclusionNode.scenarios.contains(s))
          throw new RuntimeException(s"$s came to wrong node $conclusionNode")
        val resultFn: RFn = conclusionNode.code.rfn;
        val result = safeCall(fnr, resultFn)
        val fna = makeClosureForAssertion(s.params, result)
        for (a <- s.assertions) {
          if (!fna(a.assertion))
            throw new AssertionException("\nAssertion " + a.description + " failed.\nScenario" + ExceptionScenarioPrinter.full(s) + "\nResult was " + result, s)
        }
      }
    }
    def evaluateBecauseForDecision(decision: Decision, params: List[Any]) = {
      decision match {
        case e: EngineNode =>
          e.evaluateBecause(makeClosureForBecause(params))
      }
    }
    def evaluateBecauseForScenario(c: Scenario, params: List[Any]) = {
      val fn = makeClosureForBecause(params)
      //    c.configure
      c.because match { case Some(b) => fn(b.because); case _ => throw new IllegalStateException("No because in " + c) }
    }

    def evaluateResultForScenario(c: Scenario, params: List[Any]): ROrException[R] = {
      val fnr = makeClosureForResult(params)
      c.configure
      try {
        val result = ROrException(fnr(c.actualCode.rfn));
        result
      } catch {
        case e: Throwable => ROrException(e)
      }
    }

    private def safeCall(fnr: ResultClosure, rfn: RFn): ROrException[R] =
      try {
        ROrException(fnr(rfn))
      } catch {
        case e: Throwable => ROrException(e)
      }

    //TODO This is awkward, there is the issue of what if some of the scenarios come to different conclusions. Ah but here is where in the future we can use cleverness and 
    //do a good partitioning.
    /**
     * p is the parent node. c is the constraint being added
     *  This is asking whether the parameters in p come to the same conclusion as c
     */
    private def resultsSame(l: CodeAndScenarios, c: Scenario): Boolean = {
      val resultFromScenario: ROrException[R] = evaluateResultForScenario(c, c.params)
      val result = l.scenarios match {
        case (lc :: tail) =>
          val resultFromRoot: ROrException[R] = evaluateResultForScenario(lc, c.params)
          val resultSame = resultFromScenario == resultFromRoot
          resultSame
        case _ => //so I don't have a scenario. But I have a code. 
          c.configure
          val resultFromRoot = makeClosureForResult(c.params)(l.rfn);
          val resultSame = resultFromScenario == Left(resultFromRoot)
          resultSame
      }
      result
    }

    def findWhereItGoes(root: RorN, s: Scenario): List[NodePath] = findWhereItGoes(List(), root, s)

    def findWhereItGoes(parents: List[NodePath], n: RorN, s: Scenario): List[NodePath] = {
      n match {
        case null => if (parents.isEmpty) Nil else throw new IllegalStateException(PathPrinter(parents))
        case Left(l: CodeAndScenarios) => NodePath(Left(l), true) :: parents; //we got to the bottom! The true... not sure about 
        case Right(r) =>
          evaluateBecauseForScenario(r.scenarioThatCausedNode, s.params) match {
            case true => findWhereItGoes(NodePath(n, true) :: parents, r.yes, s);
            case false => findWhereItGoes(NodePath(n, false) :: parents, r.no, s);
          }
      }
    }

    def withScenario(root: RorN, s: Scenario): RorN = {
      (root, s.because) match {
        case (null, None) =>
          logger.newRoot(s.titleString)
          Left(CodeAndScenarios(s.actualCode, List(s)));
        //        case (null, Some(_)) => throw new CannotHaveBecauseInFirstScenarioException
        case _ =>
          val path = findWhereItGoes(root, s)
          val reversedPath = path.reverse;
          withScenario(path, reversedPath, s)._1
      }
    }
    def addAssertion(fullPath: List[NodePath], codeAndScenarios: CodeAndScenarios, s: Scenario): RorN = {
      val actualResultIfUseThisScenariosCode: ROrException[R] = safeCall(makeClosureForResult(s.params), codeAndScenarios.code.rfn);
      val result = (s.expected, actualResultIfUseThisScenariosCode) match {
        case (Some(ROrException(Some(v1), None)), ROrException(Some(v2), None)) if (v1 == v2) => { logger.addScenarioFor(s.titleString, codeAndScenarios.code); Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)) }
        case (Some(ROrException(Some(_), None)), ROrException(Some(_), None)) if (fullPath.isEmpty) => throw ScenarioConflictingWithDefaultException(actualResultIfUseThisScenariosCode, s)
        case (Some(ROrException(Some(_), None)), ROrException(Some(_), None)) => throw ScenarioConflictingWithoutBecauseException(s.expected.get, actualResultIfUseThisScenariosCode, fullPath, s)
        case (Some(ROrException(Some(_), None)), ROrException(None, Some(e2))) => throw e2

        case (Some(ROrException(None, Some(e1))), ROrException(None, Some(e2))) if (e1.getClass == e2.getClass) => { logger.addScenarioFor(s.titleString, codeAndScenarios.code); Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)) }
        case (Some(ROrException(None, Some(e1))), ROrException(None, Some(e2))) => throw WrongExceptionThrownException(s, e1.getClass, e2)
        case (Some(ROrException(None, Some(e1))), ROrException(Some(v), None)) => throw NoExceptionThrownException(s, e1.getClass, v)
        case _ => throw new IllegalStateException(f"Expected: ${s.expected}\nActual: ${actualResultIfUseThisScenariosCode}")
      }
      result
    }

    def lastParent(fullPath: List[NodePath]): Option[(EngineNode, Boolean)] = {
      fullPath match {
        case _ :: NodePath(Right(n), yesNo) :: _ => Some((n, yesNo))
        case _ => None
      }
    }

    def checkDoesntMatch(codeAndScenarios: CodeAndScenarios, scenarioBeingAdded: Scenario) {
      for (scenario <- codeAndScenarios.scenarios) {
        scenario.configure
        val result = makeClosureForBecause(scenario.params)(scenarioBeingAdded.because.get.because)
        if (result)
          throw ScenarioConflictException(scenario, scenarioBeingAdded)
      }
    }

    //If the boolean is false the RorN returns is the new child. If it is true, then it is the new parent (and therefore includes the child)
    //The only time the boolean in the result is true, is when adding and or to the parent
    def withScenario(fullPath: List[NodePath], path: List[NodePath], s: Scenario): (RorN, Boolean) = {
      implicit def toTuple(rOrN: RorN) = (rOrN, false)
      def newCnC = CodeAndScenarios(s.actualCode, List(s))
      val result: (RorN, Boolean) = (path, s.because) match {

        //Should only get here if root is null... this is now a legacy clause, as it will never be null 
        case (Nil, _) => { logger.newRoot(s.titleString); Left(CodeAndScenarios(s.actualCode, List(s))) }

        //walking down to the bottom
        case (NodePath(Right(node), true) :: tail, _) =>
          val recurse = withScenario(fullPath, tail, s);
          if (recurse._2) recurse._1 else Right(node.copy(yes = recurse._1));
        case (NodePath(Right(node), false) :: tail, _) =>
          val recurse = withScenario(fullPath, tail, s);
          if (recurse._2) recurse._1 else Right(node.copy(no = recurse._1));

        //Assertions are scenarios without a because
        case (NodePath(Left(codeAndScenarios), _) :: Nil, None) =>
          val optParent = lastParent(fullPath)
          optParent match {
            case None if (codeAndScenarios.default) => { logger.newRoot(s.titleString); Left(CodeAndScenarios(s.actualCode, List(s))) }
            case _ => addAssertion(fullPath, codeAndScenarios, s)
          }

        //I have a because. The because must be good enough 
        case (NodePath(Left(codeAndScenarios), _) :: Nil, Some(because)) =>
          val optParent = lastParent(fullPath)

          val actualResultIfUseThisScenariosCode: ROrException[R] = safeCall(makeClosureForResult(s.params), codeAndScenarios.code.rfn);
          val expectedSame = Some(actualResultIfUseThisScenariosCode) == s.expected
          (expectedSame, optParent) match {
            case (true, None) =>
              logger.mergeRoot(s.titleString)
              Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios))
            case (true, Some((lastParent, yesNo))) =>
              val newBecause = s.because.collect { case b => b :: lastParent.because }.getOrElse(lastParent.because)
              logger.merge(lastParent.scenarioThatCausedNode.titleString, s.titleString, yesNo)
              if (yesNo)
                (Right(lastParent.copy(because = newBecause, yes = Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)))), true)
              else
                (Right(lastParent.copy(because = newBecause, no = Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)))), true)
            case (false, Some((lastParent, yesNo))) =>
              checkDoesntMatch(codeAndScenarios, s)
              logger.addingUnder(s.titleString, yesNo, lastParent.scenarioThatCausedNode.titleString);
              Right(EngineNode(List(because), s.params, Left(newCnC), Left(codeAndScenarios), s))
            case (false, None) =>
              checkDoesntMatch(codeAndScenarios, s)
              logger.addFirstIfThenElse(s.titleString);
              Right(EngineNode(List(because), s.params, Left(newCnC), Left(codeAndScenarios), s))
          }

        case (p :: tail, _) => throw new IllegalStateException(f"Path = ${path.toString}\nscenario = $s")
      }

      result
    }
  }

  abstract class Engine(val title: Option[String], val description: Option[String], val children: ReportableList, val optCode: Option[Code], val priority: Option[Int] = None, val references: List[Reference], val documents: List[Document], val paramDetails: List[ParamDetail]) extends BuildEngine with ReportableHolder with org.cddcore.engine.Engine[R] {
    import org.cddcore.engine.Engine._
    def this(builderData: ScenarioBuilderData) = this(builderData.title, builderData.description, builderData.childrenModifiedForBuild, builderData.optCode, builderData.priority, builderData.references, builderData.documents, builderData.paramDetails.reverse)

    def logger = EngineUniverse.this.logger
    override def templateName = "Engine"
    lazy val childEngines: List[ChildEngine] = all(classOf[ChildEngine])
    lazy val useCases: List[UseCase] = all(classOf[UseCase])
    lazy val scenarios: List[Scenario] = all(classOf[Scenario])

    if (!testing)
      validateScenarios(root, scenarios)

    def findConclusionFor(params: List[Any]): Conclusion = findConclusionFor(makeClosureForBecause(params), root)
    def evaluateConclusion(params: List[Any], conclusion: Conclusion): R = {
      conclusion match {
        case c: CodeAndScenarios => makeClosureForResult(params)(c.code.rfn)
      }
    }
    def evaluateConclusionNoException(params: List[Any], conclusion: Conclusion) = {
      try conclusion match {
        case c: CodeAndScenarios => ROrException(makeClosureForResult(params)(c.code.rfn))
      } catch { case e: Throwable => ROrException(e) }
    }

    def constructionString: String = constructionString(defaultRoot, scenarios, new DefaultIfThenPrinter)

    def logParams(params: => List[Any]) = {
      logger.executing(params)
      call(this, params)
    }

    def logResult(fn: => (Conclusion, R)): R = {
      val (conclusion, result) = fn;
      endCall(conclusion, result)
      logger.result(result)
      result
    }
    def logFailed(fn: => (Conclusion, Throwable)) {
      val (conclusion, exception) = fn;
      failedCall(conclusion, exception)
    }

    def constructionString(root: RorN, cs: List[Scenario], printer: IfThenPrinter) =
      increasingScenariosList(cs).reverse.map((cs) =>
        try {
          val c = cs.head
          val title = "Adding " + c + "\n"
          val (r, s) = buildRoot(root, cs.reverse)
          title + toStringWith(List(), r, printer)
        } catch {
          case e: Throwable => e.getClass() + "\n" + e.getMessage()
        }).mkString("\n")

    def applyParams(root: RorN, params: List[Any], log: Boolean): R = {
      if (log) logParams(params)
      val bec = makeClosureForBecause(params)
      val conclusion = evaluate(bec, root, log);
      try {
        val rfn = conclusion.code.rfn
        val result = makeClosureForResult(params)(rfn)
        if (log) logResult((conclusion, result))
        result
      } catch {
        case e: Throwable =>
          if (log) logFailed((conclusion, e)); throw e
      }
    }

    private def checkScenario(root: RorN, c: Scenario) {
      validateBecause(c);
      validateScenario(root, c);
      val actualFromEngine = applyParams(root, c.params, false);
      if (actualFromEngine != c.expected)
        throw new EngineResultException("Wrong result for " + c.actualCode.description + " for " + c.params + "\nActual: " + actualFromEngine + "\nExpected: " + c.expected);
    }

    def validateScenarios {
      //      println("In validate scenarios")
      for (c <- scenarios)
        validateScenario(root, c)
    }

  }

}
