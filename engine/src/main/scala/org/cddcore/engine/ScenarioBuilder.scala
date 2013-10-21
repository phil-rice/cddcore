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

class ExceptionAddingScenario(msg: String, t: Throwable) extends EngineException(msg, t)

//TODO Very unhappy with EngineTest. It's a global variable. I don't know how else to interact with Junit though
object EngineTest {
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
trait EngineTypes[R] {
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

trait EngineUniverse[R] extends EngineTypes[R] {

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
      s"Existing: ${existing.descriptionString}\nBeing Added: ${s.descriptionString}\nDetailed existing:\n${existing}\nDetailed of being Added:\n${s}"
    def full(s: Scenario) = s + "\nDetailed:\n  " + params(s)
    def params(s: Scenario) = s.paramPrinter(s.params)
    def scenario2Str(s: Scenario) =
      if (fullScenario)
        s.descriptionString
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
  class CannotDefineDefaultTwiceException(msg: String, val original: Code, val beingAdded: Code) extends EngineException(msg, null)
  object CannotDefineDefaultTwiceException {
    def apply(original: Code, beingAdded: Code) = new CannotDefineDefaultTwiceException(s"Original Code:\n${original}\nBeingAdded\n${beingAdded}", original, beingAdded);
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

  case class CodeAndScenarios(val code: Code, val scenarios: List[Scenario] = List(), default: Boolean = false) {

    def addedBy = scenarios match {
      case Nil => None
      case _ => Some(scenarios.last)
    }
    override def toString() = {
      val defaultString = if (default) "default, " else ""
      getClass.getSimpleName + "(" + defaultString + code + ":" + scenarios.map(_.description).mkString(",") + ")";
    }
  }

  case class EngineNode(val because: List[Because[B]], inputs: List[Any], yes: RorN, no: RorN, scenarioThatCausedNode: Scenario) {
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
        if (fn(b.because)) return true
      false

    }
    override def toString = getClass().getSimpleName() + "(" + becauseString + " => " + yes.toString() + " =/> " + no.toString() + " / " + scenarioThatCausedNode.description + ")";
  }

  abstract class BuilderNode(val description: Option[String], val expected: Option[ROrException[R]], val optCode: Option[Code], val priority: Int) {

  }

  case class Scenario(
    locator: String,
    description: Option[String],
    params: List[Any],
    paramPrinter: LoggerDisplayProcessor,
    expected: Option[ROrException[R]] = None,
    optCode: Option[Code] = None,
    because: Option[Because[B]] = None,
    assertions: List[Assertion[A]] = List(),
    configuration: Option[CfgFn] = None,
    priority: Int = 0) {

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
    def descriptionString = description.getOrElse(locator)
    override def toString =
      s"Scenario(${descriptionString}, ${params.map(paramPrinter).mkString(",")}, because=${becauseString}, expected=${logger(expected.getOrElse("<N/A>"))})"
  }

  case class ScenarioExceptionMap(map: Map[Scenario, Throwable] = Map(), first: Option[Throwable] = None) {
    def size = map.size
    def values = map.values
    def +(x: (Scenario, Throwable)) = ScenarioExceptionMap(map + x, Some(first.getOrElse(x._2)))
    def apply(s: Scenario) = map(s)
    def contains(s: Scenario) = map.contains(s)
  }
  val scenarioLens = Lens[RealScenarioBuilder, Scenario](
    (b) => b.useCases match {
      case u :: ut => u.scenarios match {
        case s :: st => s
        case _ => throw new NeedScenarioException
      }
      case _ => throw new NeedUseCaseException
    },
    (b, s) => b.useCases match {
      case u :: ut => u.scenarios match {
        case sold :: st => b.withCases(b.description, u.copy(scenarios = s :: st) :: ut, b.defaultCode)
        case _ => throw new NeedScenarioException
      }
      case _ => throw new NeedUseCaseException
    })
  case class UseCase(description: Option[String], scenarios: List[Scenario], expected: Option[ROrException[R]])

  trait ScenarioBuilder extends ScenarioWalker {
    def validateBecause(s: Scenario) = {
      s.configure
      s.because match {
        case Some(b) =>
          if (!makeClosureForBecause(s.params).apply(b.because))
            throw new ScenarioBecauseException(s.becauseString + " is not true for " + ExceptionScenarioPrinter.full(s) + "\n", s);
        case None =>
      }
    }

    def description: Option[String]

    def defaultCode: Option[Code]

    def useCases: List[UseCase]

    def withCases(description: Option[String], useCases: List[UseCase], defaultCode: Option[Code]): RealScenarioBuilder;
    def description(description: String) = withCases(Some(description), useCases, defaultCode)
    def thisAsBuilder: RealScenarioBuilder

    def checkExpectedEmpty(s: Scenario) = if (s.expected.isDefined) throw new IllegalStateException("Cannot specify expected a second time")
    def checkCodeEmpty(s: Scenario) = if (s.optCode != None) throw new IllegalStateException("Cannot specify code a second time")
    def expectException[E <: Throwable](e: E, comment: String = "") = (useCases match {
      case u :: ut => u.scenarios match {
        case s :: st => scenarioLens.mod(thisAsBuilder, (s) => { checkExpectedEmpty(s); s.copy(expected = Some(ROrException(e))) })
        case _ => withCases(description, UseCase(u.description, u.scenarios, Some(ROrException[R](e))) :: ut, defaultCode)
      }
      case _ => throw new NeedUseCaseException
    })
    def expected(e: R) = (useCases match {
      case u :: ut => u.scenarios match {
        case s :: st => scenarioLens.mod(thisAsBuilder, (s) => { checkExpectedEmpty(s); s.copy(expected = Some(ROrException(e))) })
        case _ => withCases(description, UseCase(u.description, u.scenarios, Some(ROrException[R](e))) :: ut, defaultCode)
      }
      case _ => throw new NeedUseCaseException
    })
    def because(b: Because[B], comment: String = "") = scenarioLens.mod(thisAsBuilder,
      (s) => {
        validateBecause(s);
        s.copy(because = Some(b.copy(comment = comment)))
      })

    def withDefaultCode(code: Code): RealScenarioBuilder = {
      if (defaultCode.isDefined)
        throw CannotDefineDefaultTwiceException(defaultCode.get, code)
      else
        withCases(description, useCases, Some(code))
    }
    protected def withUseCase(useCaseDescription: String) =
      withCases(description, UseCase(Some(useCaseDescription), List(), None) :: useCases, defaultCode);
    def useCase(useCaseDescription: String) = withUseCase(useCaseDescription)
    def code(c: CodeFn[B, RFn, R], comment: String = "") = scenarioLens.mod(thisAsBuilder, (s) => { checkCodeEmpty(s); s.copy(optCode = Some(c.copy(comment = comment))) })
    def configuration[K](cfg: CfgFn) = scenarioLens.mod(thisAsBuilder, (s) => s.copy(configuration = Some(cfg)))
    def priority(priority: Int) = scenarioLens.mod(thisAsBuilder, (s) => s.copy(priority = priority))
    def assertion(a: Assertion[A], comment: String = "") = scenarioLens.mod(thisAsBuilder, (s) => s.copy(assertions = a.copy(comment = comment) :: s.assertions))

    def useCasesForBuild: List[UseCase] =
      useCases.map(u =>
        UseCase(
          u.description,
          u.scenarios.map((s) =>
            (s.expected , u.expected) match {
              case (None, Some(e)) => s.copy(expected = Some(e))
              case _ => s
            }).reverse,
          u.expected)).reverse;

    def scenariosForBuild: List[Scenario] =
      useCasesForBuild.flatMap((u) => u.scenarios);

    def newScenario(scenarioDescription: String, params: List[Any]) = useCases match {
      case h :: t => {
        val descriptionString = if (scenarioDescription == null) None else Some(scenarioDescription);
        withCases(description, UseCase(h.description, Scenario(s"${h.description.getOrElse("")}[${h.scenarios.size}]", descriptionString, params, logger) :: h.scenarios, h.expected) :: t, defaultCode)
      }
      case _ => throw new NeedUseCaseException
    }
  }

  trait IfThenPrinter {
    def resultPrint: (String, CodeAndScenarios) => String
    def ifPrint: (String, EngineNode) => String
    def elsePrint: (String, EngineNode) => String
    def endPrint: (String, EngineNode) => String
    def titlePrint: (String, Scenario) => String
  }

  trait ScenarioVisitor {
    def start(engineDescription: Option[String])
    def visitUseCase(useCaseindex: Int, u: UseCase)
    def visitScenario(useCaseindex: Int, u: UseCase, scenarioIndex: Int, s: Scenario)
    def visitUseCaseEnd(u: UseCase)
    def end
  }

  trait ScenarioWalker {
    def useCases: List[UseCase];
    def description: Option[String]
    def walkScenarios(v: ScenarioVisitor, reverse: Boolean) {
      def actual[X](list: List[X]) = if (reverse) list.reverse else list;
      v.start(description)
      for ((u, ui) <- actual(useCases).zipWithIndex) {
        v.visitUseCase(ui, u)
        for ((s, si) <- actual(u.scenarios).zipWithIndex)
          v.visitScenario(ui, u, si, s)
        v.visitUseCaseEnd(u)
      }
      v.end
    }
  }

  trait EvaluateEngine {

    def evaluate(fn: BecauseClosure, n: RorN, log: Boolean = true): RFn = {
      val result = n match {
        case Left(r) =>
          val result = r.code.rfn
          result
        case Right(n) => evaluate(fn, n, log)
      }
      result
    }

    private def evaluate(fn: BecauseClosure, n: EngineNode, log: Boolean): RFn = {
      val condition = n.evaluateBecause(fn)
      if (log)
        logger.evaluating(n.because, condition)
      condition match {
        case false => evaluate(fn, n.no, log);
        case true => evaluate(fn, n.yes, log);
      }
    }
  }
  trait EngineToString {
    def root: RorN
    def buildRoot(root: RorN, scenarios: List[Scenario]): (RorN, ScenarioExceptionMap)

    def toString(indent: String, root: RorN): String = {
      root match {
        case null => indent + "null"
        case Left(result) => indent + result.pretty + "\n"
        case Right(node) =>
          indent + "if(" + node.because.pretty + ")\n" +
            toString(indent + " ", node.yes) +
            indent + "else\n" +
            toString(indent + " ", node.no)
      }
    }
    override def toString(): String = toString("", root)

    def toStringWithScenarios(): String = toStringWithScenarios("", root);

    def increasingScenariosList(cs: List[Scenario]): List[List[Scenario]] =
      cs.foldLeft(List[List[Scenario]]())((a, c) => (a match {
        case (h :: t) => (c :: a.head) :: a;
        case _ => List(List(c))
      }))

    class DefaultIfThenPrinter extends IfThenPrinter {
      def resultPrint = (indent, result) => indent + result.pretty + ":" + result.scenarios.map((s) => s.descriptionString).mkString(",") + "\n"
      def ifPrint = (indent, node) => indent + "if(" + node.prettyString + ")\n"
      def elsePrint = (indent, node) => indent + "else\n";
      def endPrint = (indent, node) => "";
      def titlePrint: (String, Scenario) => String = (indent, scenario) => "";
    }

    def toStringWith(indent: String, root: RorN, printer: IfThenPrinter): String = {
      root match {
        case null => "Could not toString as root as null. Possibly because of earlier exceptions"
        case Left(result) => printer.resultPrint(indent, result)
        case Right(node) =>
          val ifString = printer.ifPrint(indent, node)
          val yesString = toStringWith(indent + " ", node.yes, printer)
          val elseString = printer.elsePrint(indent, node)
          val noString = toStringWith(indent + " ", node.no, printer)
          val endString = printer.endPrint(indent, node)
          val result = ifString + yesString + elseString + noString + endString
          return result
      }
    }

    def toStringWithScenarios(indent: String, root: RorN): String =
      toStringWith(indent, root, new DefaultIfThenPrinter())

    def constructionString(root: RorN, cs: List[Scenario], printer: IfThenPrinter) =
      increasingScenariosList(cs).reverse.map((cs) =>
        try {
          val c = cs.head
          val title = printer.titlePrint("", c)
          val (r, s) = buildRoot(root, cs.reverse)
          title + toStringWith("", r, printer)
        } catch {
          case e: Throwable => e.getClass() + "\n" + e.getMessage()
        }).mkString("\n")
  }

  trait BuildEngine extends EvaluateEngine with EngineToString {
    def validateBecause(s: Scenario) {
      s.configure
      s.because match {
        case Some(b) =>
          if (!makeClosureForBecause(s.params).apply(b.because))
            throw new ScenarioBecauseException(s.becauseString + " is not true for " + ExceptionScenarioPrinter.full(s), s);
        case None =>
      }
    }

    protected def checkExpectedMatchesAction(root: RorN, s: Scenario) {
      s.configure
      val bec = makeClosureForBecause(s.params)
      val rFn: RFn = evaluate(bec, root, false)
      val actualFromScenario: ROrException[R] = safeCall(makeClosureForResult(s.params), rFn)
      if (s.expected.isEmpty)
        throw NoExpectedException(s)
      if (actualFromScenario != s.expected)
        throw new ScenarioResultException("Wrong result for " + s.actualCode.description + " for " + ExceptionScenarioPrinter(s) + "\nActual: " + actualFromScenario + "\nExpected: " + s.expected.getOrElse("<N/A>") + "\nRoot:\n" + toString("", root), s);
    }

    protected def validateScenario(root: RorN, s: Scenario) {
      s.configure
      val bec = makeClosureForBecause(s.params)
      val rFn: RFn = evaluate(bec, root, false)
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
      val sorted = cs.sortBy(-_.priority)
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
              buildFromScenarios(root, tail, seMap + (c -> e))
          }

        case _ => (root, seMap);
      }
    }

    def validateScenarios(root: RorN, scenarios: List[Scenario]) {
      if (root == null)
        if (EngineTest.testing)
          return
        else
          throw new NullPointerException("Cannot validate scenario as root doesn't exist")
      for (s <- scenarios) {
        s.configure
        val bc = makeClosureForBecause(s.params)
        val fnr = makeClosureForResult(s.params)
        val resultFn: RFn = evaluate(bc, root, false);
        val result = safeCall(fnr, resultFn)
        val fna = makeClosureForAssertion(s.params, result)
        for (a <- s.assertions) {
          if (!fna(a.assertion))
            throw new AssertionException("\nAssertion " + a.description + " failed.\nScenario" + ExceptionScenarioPrinter.full(s) + "\nResult was " + result, s)
        }
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
          logger.newRoot(s.descriptionString)
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
        case (Some(ROrException(Some(v1), None)), ROrException(Some(v2), None)) if (v1 == v2) => { logger.addScenarioFor(s.descriptionString, codeAndScenarios.code); Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)) }
        case (Some(ROrException(Some(_), None)), ROrException(Some(_), None)) if (fullPath.isEmpty) => throw ScenarioConflictingWithDefaultException(actualResultIfUseThisScenariosCode, s)
        case (Some(ROrException(Some(_), None)), ROrException(Some(_), None)) => throw ScenarioConflictingWithoutBecauseException(s.expected.get, actualResultIfUseThisScenariosCode, fullPath, s)
        case (Some(ROrException(Some(_), None)), ROrException(None, Some(e2))) => throw e2

        case (Some(ROrException(None, Some(e1))), ROrException(None, Some(e2))) if (e1.getClass == e2.getClass) => { logger.addScenarioFor(s.descriptionString, codeAndScenarios.code); Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)) }
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
        case (Nil, _) => { logger.newRoot(s.descriptionString); Left(CodeAndScenarios(s.actualCode, List(s))) }

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
            case None if (codeAndScenarios.default) => { logger.newRoot(s.descriptionString); Left(CodeAndScenarios(s.actualCode, List(s))) }
            case _ => addAssertion(fullPath, codeAndScenarios, s)
          }

        //I have a because. The because must be good enough 
        case (NodePath(Left(codeAndScenarios), _) :: Nil, Some(because)) =>
          val optParent = lastParent(fullPath)

          val actualResultIfUseThisScenariosCode: ROrException[R] = safeCall(makeClosureForResult(s.params), codeAndScenarios.code.rfn);
          val expectedSame = Some( actualResultIfUseThisScenariosCode) == s.expected
          (expectedSame, optParent) match {
            case (true, None) =>
              logger.mergeRoot(s.descriptionString)
              Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios))
            case (true, Some((lastParent, yesNo))) =>
              val newBecause = s.because.collect { case b => b :: lastParent.because }.getOrElse(lastParent.because)
              logger.merge(lastParent.scenarioThatCausedNode.descriptionString, s.descriptionString, yesNo)
              if (yesNo)
                (Right(lastParent.copy(because = newBecause, yes = Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)))), true)
              else
                (Right(lastParent.copy(because = newBecause, no = Left(codeAndScenarios.copy(scenarios = s :: codeAndScenarios.scenarios)))), true)
            case (false, Some((lastParent, yesNo))) =>
              checkDoesntMatch(codeAndScenarios, s)
              logger.addingUnder(s.descriptionString, yesNo, lastParent.scenarioThatCausedNode.descriptionString);
              Right(EngineNode(List(because), s.params, Left(newCnC), Left(codeAndScenarios), s))
            case (false, None) =>
              checkDoesntMatch(codeAndScenarios, s)
              logger.addFirstIfThenElse(s.descriptionString);
              Right(EngineNode(List(because), s.params, Left(newCnC), Left(codeAndScenarios), s))
          }

        case (p :: tail, _) => throw new IllegalStateException(f"Path = ${path.toString}\nscenario = $s")
      }

      result
    }

  }
  abstract class Engine(val description: Option[String], val defaultCode: Option[Code]) extends BuildEngine with ScenarioWalker {
    def defaultRoot: RorN = defaultCode match {
      case Some(code) => Left(new CodeAndScenarios(code, List(), true))
      case _ => Left(new CodeAndScenarios(rfnMaker(Left(() =>
        new UndecidedException)), List(), true))
    }
    def useCases: List[UseCase];
    lazy val scenarios: List[Scenario] = useCases.flatMap(_.scenarios)

    private val rootAndExceptionMap = buildRoot(defaultRoot, scenarios)
    val root: RorN = rootAndExceptionMap._1
    val scenarioExceptionMap: ScenarioExceptionMap = rootAndExceptionMap._2

    if (!EngineTest.testing)
      validateScenarios(root, scenarios)

    def constructionString: String =
      constructionString(defaultRoot, scenarios, new DefaultIfThenPrinter {
        override def titlePrint = (indent, scenario) => s"${indent}Adding ${scenario}\n";
      })

    def logParams(p: Any*) =
      logger.executing(p.toList)

    def logResult(fn: => R): R = {
      val result: R = fn;
      logger.result(result)
      result
    }

    def buildRoot(root: RorN, scenarios: List[Scenario]): (RorN, ScenarioExceptionMap) = {
      scenarios match {
        case s :: rest =>
          val newRootAndExceptionMap = buildFromScenarios(root, scenarios, ScenarioExceptionMap());
          val (newRoot, seMap) = newRootAndExceptionMap
          if (!EngineTest.testing) {
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

    def applyParam(root: RorN, params: List[Any], log: Boolean): R = {
      val bec = makeClosureForBecause(params)
      val rfn = evaluate(bec, root, log)
      makeClosureForResult(params)(rfn)
    }

    private def checkScenario(root: RorN, c: Scenario) {
      validateBecause(c);
      validateScenario(root, c);
      val actualFromEngine = applyParam(root, c.params, false);
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
