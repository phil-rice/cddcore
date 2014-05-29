package org.cddcore.engine.builder

import org.cddcore.utilities._
import org.cddcore.engine._

object BuildEngine {
  def initialNodes[Params, BFn, R, RFn] = List(new EngineDescription[Params, BFn, R, RFn]())
  def initialNodes[Params, BFn, R, RFn, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR) =
    List(FoldingEngineDescription[Params, BFn, R, RFn, FullR](initialValue = new CodeHolder(() => initialValue, initialValue.toString), foldingFn = foldingFn))

  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) = Conclusion[Params, BFn, R, RFn](code, List())
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] = new CodeHolder((p: P) =>
    throw UndecidedException(p), "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] = new CodeHolder((p1: P1, p2: P2) =>
    throw UndecidedException(p1, p2), "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] = new CodeHolder((p1: P1, p2: P2, p3: P3) =>
    throw UndecidedException(p1, p2, p3), "throws Undecided Exception")

  private def expectedValue[R](x: Either[Exception, R]): R = x match {
    case Left(e) =>
      throw e
    case Right(r) => r
  }
  private def expectedToString[R](x: Either[Exception, R]) = x match {
    case Left(e) => e.toString()
    case Right(r) => r.toString
  }

  def expectedToCode1[P, R]: Either[Exception, R] => CodeHolder[(P) => R] = (x) => new CodeHolder((p) => expectedValue(x), expectedToString(x))
  def expectedToCode2[P1, P2, R]: Either[Exception, R] => CodeHolder[(P1, P2) => R] = (x) => new CodeHolder((p1, p2) => expectedValue(x), expectedToString(x))
  def expectedToCode3[P1, P2, P3, R]: Either[Exception, R] => CodeHolder[(P1, P2, P3) => R] = (x) => new CodeHolder((p1, p2, p3) => expectedValue(x), expectedToString(x))

  //  def construct1[P, R, E] =  (dt: DecisionTree[P, (P) => Boolean, R, (P) => R ], 
  //      asRequirement: BuilderNodeHolder[R, (P)=> R], buildExceptions:ExceptionMap) => Engine1(asRequirement, buildExceptions)

  def builderEngine1[P, R] = new SimpleBuildEngine1[P, R]
  def builderEngine2[P1, P2, R] = new SimpleBuildEngine2[P1, P2, R]
  def builderEngine3[P1, P2, P3, R] = new SimpleBuildEngine3[P1, P2, P3, R]
  def folderBuilderEngine1[P, R, FullR] = new FoldingBuildEngine1[P, R, FullR]
  def folderBuilderEngine2[P1, P2, R, FullR] = new FoldingBuildEngine2[P1, P2, R, FullR]
  def folderBuilderEngine3[P1, P2, P3, R, FullR] = new FoldingBuildEngine3[P1, P2, P3, R, FullR]
  def validateScenario[Params, BFn, R, RFn] = new SimpleValidateScenario[Params, BFn, R, RFn]
}

abstract class SimpleBuildEngine[Params, BFn, R, RFn, E <: EngineTools[Params, BFn, R, RFn]](
  val defaultRoot: CodeHolder[RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn])(implicit val ldp: CddDisplayProcessor)
  extends BuildEngineFromTests[Params, BFn, R, RFn, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, BFn, R, RFn]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens, BuildEngine.validateScenario)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[Params, BFn, R, RFn]
}

trait BuildEngineFromTests[Params, BFn, R, RFn, E <: EngineTools[Params, BFn, R, RFn]] extends BuildEngine[Params, BFn, R, RFn, R, E] {
  def constructEngine(asRequirement: EngineRequirement[Params, BFn, R, RFn], dt: DecisionTree[Params, BFn, R, RFn], exceptionMap: ExceptionMap, ldp: CddDisplayProcessor): E
  def buildEngine(requirement: EngineRequirement[Params, BFn, R, RFn], buildExceptions: ExceptionMap, ldp: CddDisplayProcessor) = {
    requirement match {
      case ed: EngineDescription[Params, BFn, R, RFn] =>
        val (dt, exceptionMap, modifiedRequirement) = buildTree(ed, buildExceptions)
        constructEngine(modifiedRequirement.copy(tree = Some(dt)), dt, exceptionMap, ldp)
    }
  }
}

trait BuildEngine[Params, BFn, R, RFn, FullR, E <: EngineTools[Params, BFn, R, RFn]] {
  type DT = DecisionTree[Params, BFn, R, RFn]
  type DN = DecisionTreeNode[Params, BFn, R, RFn]
  type Dec = Decision[Params, BFn, R, RFn]
  type Conc = Conclusion[Params, BFn, R, RFn]
  type S = Scenario[Params, BFn, R, RFn]

  def evaluateTree: EvaluateTree[Params, BFn, R, RFn]
  def defaultRoot: CodeHolder[RFn]
  def blankTree(engineDescription: BuilderNode[Params, BFn, R, RFn]): DT = new SimpleDecisionTree(Conclusion(engineDescription.code.getOrElse(defaultRoot), List()), rootIsDefault = true)
  def builderWithModifyChildrenForBuild: BuilderWithModifyChildrenForBuild[Params, BFn, R, RFn]
  def expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]
  def decisionTreeLens: DecisionTreeLens[Params, BFn, R, RFn]
  val mc = evaluateTree.makeClosures
  val validator = evaluateTree.validator

  def buildEngine(requirement: EngineRequirement[Params, BFn, R, RFn], buildExceptions: ExceptionMap, ldp: CddDisplayProcessor): E

  protected def buildTree[ED <: BuilderNodeAndHolder[Params, BFn, R, RFn]](asRequirement: ED, buildExceptions: ExceptionMap): (DT, ExceptionMap, ED) = {
    val modifiedRequirements = builderWithModifyChildrenForBuild.modifyChildrenForBuild(asRequirement)
    val scenarios = modifiedRequirements.all(classOf[S]).toList.sorted(Ordering.by((x: S) => (-x.priority.getOrElse(0), x.textOrder)))
    val (newDecisionTree, newENap) = scenarios.foldLeft((blankTree(asRequirement), buildExceptions))((acc, s) => {
      val (dt, exceptionMap) = acc
      try {
        validator.preValidateScenario(mc, s)
        val newTree = addScenario(dt, s)
        validator.checkAssertions(evaluateTree, newTree, s)
        (newTree, exceptionMap)
      } catch {
        case e: Exception =>
          Engine.testing match {
            case false => throw e;
            case true => (dt, exceptionMap + (s -> e))
          }
      }
    })
    val finalEMap = scenarios.foldLeft(newENap)((eMap, s) =>
      try { validator.checkCodeComesToExpected(evaluateTree, s); eMap }
      catch { case e: Exception => eMap + (s -> e) })
    if (!Engine.testing)
      scenarios.foreach((x) => validator.postValidateScenario(evaluateTree, newDecisionTree, x))
    (newDecisionTree, finalEMap, modifiedRequirements)
  }
  protected def addScenario(tree: DT, scenario: S): DT = {
    val lens = evaluateTree.lens
    import lens._
    def actualFromNewScenario(c: Conc) = mc.safeEvaluateResult(c.code.fn, scenario)

    val concL = evaluateTree.findLensToConclusion(tree.root, scenario)
    val concLToConclusionL = concL.andThen(toConclusionL)
    val oldConclusion = concLToConclusionL.get(tree)
    val actual = actualFromNewScenario(oldConclusion)
    val expected = scenario.expected.get
    val comesToSameConclusion = Reportable.compare(expected, actual)
    def newConclusion = Conclusion(code = scenario.actualCode(expectedToCode), scenarios = List(scenario))
    def addAssertion(lensToNode: Lens[DT, Conc]) = lensToNode.mod(tree, (c) => c.copy(scenarios = c.scenarios :+ scenario))
    def addOrRule = {
      val newConclusion = oldConclusion.copy(scenarios = oldConclusion.scenarios :+ scenario)
      evaluateTree.findLensToLastDecisionNode(tree.root, scenario) match {
        case Some(lensToParent) => {
          val existingDecision = lensToParent.get(tree);
          existingDecision.no.scenarios.filter((s) => mc.evaluateBecause(scenario, s)) match {
            case Nil => {
              val newDecision = existingDecision.copy(yes = newConclusion, because = existingDecision.because :+ scenario.because.get)
              lensToParent.set(tree, newDecision)
            }
            case wouldBeBrokenByOrRule: List[S] => throw ScenarioCausingProblemWithOrRuleException(wouldBeBrokenByOrRule, scenario)
          }
        }
        case _ => concLToConclusionL.set(tree, newConclusion)
      }

    }
    def addDecisionNodeTo = {
      concL.mod(tree, (c) => c match {
        case c: Conc =>
          c.scenarios.filter((s) => mc.evaluateBecause(scenario, s)) match {
            case Nil => Decision(List(scenario.because.get), yes = newConclusion, no = c, scenarioThatCausedNode = scenario)
            case brokenScenarios => throw ScenarioConflictAndBecauseNotAdequateException(concL, expected, actual, brokenScenarios, scenario)
          }
        case _ => throw new IllegalStateException
      })
    }

    val result: DT =
      (comesToSameConclusion, scenario.because, tree.rootIsDefault) match {
        case (false, None, true) => concLToConclusionL.set(tree, newConclusion)
        case (true, None, _) => addAssertion(concL.andThen(toConclusionL))
        case (true, Some(_), _) if !scenario.code.isDefined && oldConclusion.scenarios.foldLeft(true)((acc, s) => acc && !s.code.isDefined) => addOrRule
        case (_, None, _) =>
          oldConclusion.scenarios match {
            case Nil => throw ScenarioConflictingWithDefaultAndNoBecauseException(concL, actual, expected, scenario)
            case existing =>
              throw ScenarioConflictingWithoutBecauseException(concL, actual, expected, existing, scenario)
          }
        case _ => addDecisionNodeTo
      }
    result
  }
}

