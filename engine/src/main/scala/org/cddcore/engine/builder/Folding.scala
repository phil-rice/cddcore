package org.cddcore.engine.builder

import org.cddcore.engine._
import org.cddcore.utilities._

trait BuildFoldingEngine[Params, BFn, R, RFn, FullR, F <: EngineTools[Params, BFn, R, RFn], E <: EngineTools[Params, BFn, R, RFn]] extends BuildEngine[Params, BFn, R, RFn, FullR, F] {
  def constructFoldingEngine(
    requirement: EngineRequirement[Params, BFn, R, RFn],
    engines: List[EngineFromTests[Params, BFn, R, RFn]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR,
    ldp: CddDisplayProcessor): F
  def buildChildEngine: BuildEngine[Params, BFn, R, RFn, R, E]

  def buildEngine(r: EngineRequirement[Params, BFn, R, RFn], buildExceptions: ExceptionMap, ldp: CddDisplayProcessor) = {
    r match {
      case f: FoldingEngineDescription[Params, BFn, R, RFn, FullR] => {
        val initial = (List[EngineFromTests[Params, BFn, R, RFn]](), buildExceptions)
        if (f.nodes.isEmpty) throw new CannotHaveFoldingEngineWithoutChildEnginesException
        val (engines: List[EngineFromTests[Params, BFn, R, RFn]], exceptionMap) = f.nodes.foldLeft(initial)((acc, ed) => ed match {
          case ed: EngineDescription[Params, BFn, R, RFn] => {
            def modifiedEd = ed.copy(priority = ed.priority.orElse(f.priority), code = ed.code.orElse(f.code), expected = ed.expected.orElse(f.expected))
            val (engines, initialExceptionMap) = acc

            val (dt, exceptionMap, newRequirements) = buildTree(modifiedEd, initialExceptionMap)
            val engine = buildChildEngine.buildEngine(newRequirements, buildExceptions, ldp).asInstanceOf[EngineFromTests[Params, BFn, R, RFn]]
            (engine :: engines, exceptionMap)
          }
        });
        val requirements = f.copyNodes(engines.map(_.asRequirement))
        constructFoldingEngine(requirements, engines, exceptionMap, f.initialValue, f.foldingFn, ldp)
      }
    }
  }
}

abstract class SimpleFoldingBuildEngine[Params, BFn, R, RFn, FullR, F <: EngineTools[Params, BFn, R, RFn], E <: EngineTools[Params, BFn, R, RFn]](
  val defaultRoot: CodeHolder[RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn],
  val buildChildEngine: BuildEngine[Params, BFn, R, RFn, R, E])(implicit val ldp: CddDisplayProcessor)
  extends BuildFoldingEngine[Params, BFn, R, RFn, FullR, F, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, BFn, R, RFn]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens, BuildEngine.validateScenario)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[Params, BFn, R, RFn]
}

