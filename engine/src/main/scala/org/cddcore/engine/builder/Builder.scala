package org.cddcore.engine.builder

import scala.language.implicitConversions
import org.cddcore.utilities._
import org.cddcore.engine._

trait HasExceptionMap[R] {
  def buildExceptions: ExceptionMap
}
trait CanCopyWithNewExceptionMap[R] extends HasExceptionMap[R] {
  def copyWithNewExceptions(buildExceptions: ExceptionMap): CanCopyWithNewExceptionMap[R]
}

trait Builder[Params, R, FullR, B <: Builder[Params, R, FullR, B, E], E <: EngineTools[Params, R]]
  extends BuilderNodeHolder[Params, R]
  with CanCopyWithNewExceptionMap[R]
  with WhileBuildingValidateScenario[Params, R]
  with WithCddDisplayProcessor {
  val bl = new FullBuilderLens[Params, R, FullR, Builder[Params, R, FullR, B, E]]
  import bl._
  def expectedToCode: (Either[Exception, R]) => CodeHolder[(Params) => R]

  protected def wrap(purpose: => String, stuff: => Builder[Params, R, FullR, B, E]): B = {
    Engine.logBuild(purpose)
    wrapQuietly(stuff)
  }
  protected def wrapQuietly(stuff: => Builder[Params, R, FullR, B, E]): B = try {
    stuff.asInstanceOf[B]
  } catch {
    case e: Exception => {
      Engine.testing match {
        case true => {
          val current = currentNodeL.get(this)
          val result = builderToCanCopyWithNewExceptionMapL.andThen(exceptionMap).mod(this, (map) =>
            map + (current -> e)).asInstanceOf[B]
          result
        }
        case false => throw e
      }
    }
  }
  protected def makeClosures: MakeClosures[Params, R]

  def title(title: String): B = wrap("title", currentNodeL.andThen(asRequirementL).andThen(titleL).set(this, Some(title)))
  def description(description: String): B = wrap("description", currentNodeL.andThen(asRequirementL).andThen(descriptionL).set(this, Some(description)))
  def priority(priority: Int): B = wrap("priority", currentNodeL.andThen(asRequirementL).andThen(priorityL).set(this, Some(priority)))

  def useCase(title: String, description: String = null): B = wrap("useCase", nextUseCaseHolderL.andThen(nodesL).mod(this, (nodes: List[BuilderNode[Params, R]]) =>
    new UseCase[Params, R](Some(title), description = Option(description)) :: nodes))
  def because(because: (Params) => Boolean, description: String): B = becauseHolder(new CodeHolder[(Params) => Boolean](because, description))

  def assert(assertion: (Params, Either[Exception, R]) => Boolean, description: String) =
    wrap("assert", currentNodeL.andThen(toScenarioL).andThen(assertionL).mod(this, { _ :+ new CodeHolder(assertion, description) }))

  def becauseHolder(becauseHolder: CodeHolder[(Params) => Boolean]): B =
    wrap("because", currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(makeClosures, sn))).set(this, Some(becauseHolder)))
  def expected(r: R, title: String = null): B =
    wrap("expected", currentNodeL.andThen(expectedL).set(this, Some(Right(r))))
  def expectedAndCode(r: R, title: String = null): B = expected(r, title).codeHolder(expectedToCode(Right(r)))
  def expectException(e: Exception, title: String = null): B = wrap("expectException", currentNodeL.andThen(expectedL).set(this, Some(Left(e))))
  def reference(ref: String, document: Document = null): B =
    wrap("reference", currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, Option(document))))

  def copyNodes(nodes: List[BuilderNode[Params, R]]): B
  def codeHolder(codeHolder: CodeHolder[(Params) => R]): B = wrap("codeHolder", currentNodeL.andThen(codeL((o, n, c) => {})).set(this, Some(codeHolder)))
  def childEngine(title: String, description: String = null): B = wrap("childEngine", toFoldingEngineDescription.andThen(foldEngineNodesL).
    mod(this.asInstanceOf[B], ((n) => new EngineDescription[Params, R](title = Some(title), description = Option(description)) :: n)).asInstanceOf[Builder[Params, R, FullR, B, E]])
}

trait WhileBuildingValidateScenario[Params, R] {
  type S = Scenario[Params, R]
  type MC = MakeClosures[Params, R]
  def checkDuplicateScenario[FullR, B <: BuilderNodeHolder[Params, R]](lens: BuilderLens[Params, R, FullR, B], rootRequirement: BuilderNodeHolder[Params, R], s: S) = {
    val scenarios = lens.engineDescriptionL.get(rootRequirement).all(classOf[Scenario[Params, R]]).toList;
    if (scenarios.contains(s)) throw DuplicateScenarioException(s)
    s
  }
  def checkBecause(mc: MC, s: S)(implicit ldp: CddDisplayProcessor) = {
    s.because match {
      case Some(_) => if (!mc.evaluateBecause(s, s)) throw ScenarioBecauseException(s);
      case _ =>
    }
    s
  }
}

class SimpleValidateScenario[Params, R] extends ValidateScenario[Params, R]

trait ValidateScenario[Params, R] extends WhileBuildingValidateScenario[Params, R] {
  def preValidateScenario(mc: MC, s: S)(implicit ldp: CddDisplayProcessor) = {
    if (!s.expected.isDefined)
      throw NoExpectedException(s)
    checkBecause(mc, s)
    checkHasExpected(s)
  }
  def postValidateScenario(evaluateTree: EvaluateTree[Params, R], tree: DecisionTree[Params, R], s: S)(implicit ldp: CddDisplayProcessor) = {
    checkHaveCodeIfHaveExpectException(s)
    checkCodeComesToExpected(evaluateTree, s)
    checkAssertions(evaluateTree, tree, s)
    checkCorrectValue(evaluateTree, tree, s)
  }

  def checkHasExpected(s: S) = {
    if (s.expected.isEmpty) throw NoExpectedException(s)
    s
  }

  def checkHaveCodeIfHaveExpectException(s: S) {
    (s.expected, s.code) match {
      case (Some(Left(e)), None) => throw ScenarioShouldHaveCodeIfExpectsException(s)
      case _ =>
    }
  }
  def checkCodeComesToExpected(evaluateTree: EvaluateTree[Params, R], s: S) {
    (s.code, s.expected) match {
      case (Some(code), Some(expected)) =>
        val actual = evaluateTree.makeClosures.safeEvaluateResult(code.fn, s)
        if (!Reportable.compareAllowingExceptionToBeMoreSpecific(expected, actual))
          throw CodeDoesntProduceExpectedException(s, actual)
      case _ =>
    }
  }

  def checkAssertions(evaluateTree: EvaluateTree[Params, R], tree: DecisionTree[Params, R], s: S) = {
    s.assertions.foreach((a) => {
      val result = evaluateTree.safeEvaluate(tree, s)
      val assertionResult = try { a.fn(s.params, result) } catch { case e: Exception => throw ExceptionThrownInAssertion(s, a, e) }
      if (!assertionResult) throw AssertionException(a, s)
    })
  }
  def checkCorrectValue(evaluateTree: EvaluateTree[Params, R], tree: DecisionTree[Params, R], s: S) = {
    val actual = evaluateTree.safeEvaluate(tree, s)
    s.expected match {
      case Some(ex) => if (!Reportable.compareAllowingExceptionToBeMoreSpecific(ex, actual))
        actual match {
          case Left(cause) =>
            throw CameToWrongConclusionScenarioException(ex, actual, s, cause)
          case _ =>
            throw CameToWrongConclusionScenarioException(ex, actual, s, null)
        }
      case _ => throw NoExpectedException(s)
    }
  }
}

class SimpleBuilderWithModifyChildrenForBuild[Params, R] extends BuilderWithModifyChildrenForBuild[Params, R]

trait BuilderWithModifyChildrenForBuild[Params, R] {
  def modifyChildrenForBuild[ED <: BuilderNodeAndHolder[Params, R]](requirement: ED): ED = {
    def modifyChildAsNode(path: List[Reportable], child: BuilderNode[Params, R]) = {
      child
    }
    def firstOption[X](path: List[Reportable], fn: (BuilderNode[Params, R]) => Option[X]): Option[X] = {
      path.collect { case e: BuilderNode[Params, R] => fn(e) }.find((r) => r.isDefined).getOrElse(None)
    }
    def modifyChild(path: List[Reportable]): BuilderNode[Params, R] = {
      val nodeModified = path.head match {
        case node: BuilderNode[Params, R] => node.copyBuilderNode(
          expected = firstOption(path, _.expected),
          code = firstOption(path, _.code)).
          copyRequirement(
            priority = firstOption(path, _.priority))
        case x => x
      }
      val withChildren = nodeModified match {
        case holder: BuilderNodeAndHolder[Params, R] => holder.copyNodes(nodes = modifyChildren(path, holder))
        case x: BuilderNode[Params, R] => x
      }
      withChildren.asInstanceOf[BuilderNode[Params, R]]
    }
    def modifyChildren(path: List[Reportable], holder: BuilderNodeAndHolder[Params, R]): List[BuilderNode[Params, R]] =
      holder.nodes.map((x) => modifyChild(x :: path)).sortBy(_.textOrder)
    modifyChild(List(requirement)).asInstanceOf[ED]
  }
}