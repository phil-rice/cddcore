package org.cddcore.engine.builder

import org.cddcore.engine._
import org.cddcore.utilities._

trait BuildFoldingEngine[Params, R, FullR, F <: EngineTools[Params, R], E <: EngineTools[Params, R]] extends BuildEngine[Params, R, FullR, F] {
  def constructFoldingEngine(
    requirement: EngineRequirement[Params, R],
    engines: List[EngineFromTests[Params, R]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR,
    ldp: CddDisplayProcessor): F
  def buildChildEngine: BuildEngine[Params, R, R, E]

  def buildEngine(r: EngineRequirement[Params, R], buildExceptions: ExceptionMap, ldp: CddDisplayProcessor) = try {
    Engine.logBuild("build folding engine")
    r match {
      case f: FoldingEngineDescription[Params, R, FullR] => {
        val initial = (List[EngineFromTests[Params, R]](), buildExceptions)
        if (f.nodes.isEmpty) throw new CannotHaveFoldingEngineWithoutChildEnginesException
        val (engines: List[EngineFromTests[Params, R]], exceptionMap) = f.nodes.foldLeft(initial)((acc, ed) => ed match {
          case ed: EngineDescription[Params, R] => {
            def modifiedEd = ed.copy(priority = ed.priority.orElse(f.priority), code = ed.code.orElse(f.code), expected = ed.expected.orElse(f.expected))
            val (engines, initialExceptionMap) = acc

            val (dt, exceptionMap, newRequirements) = buildTree(modifiedEd, initialExceptionMap)
            val engine = buildChildEngine.buildEngine(newRequirements, buildExceptions, ldp).asInstanceOf[EngineFromTests[Params, R]]
            (engine :: engines, exceptionMap)
          }
        });
        val requirements = f.copyNodes(engines.map(_.asRequirement))
        constructFoldingEngine(requirements, engines, exceptionMap, f.initialValue, f.foldingFn, ldp)
      }
    }
  } finally {
    Engine.logBuild("end of build folding engine")

  }
}

abstract class SimpleFoldingBuildEngine[Params, R, FullR, F <: EngineTools[Params, R], E <: EngineTools[Params, R]](
  val defaultRoot: CodeHolder[(Params) => R],
  makeClosures: MakeClosures[Params, R],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[(Params) => R],
  val buildChildEngine: BuildEngine[Params, R, R, E])(implicit val ldp: CddDisplayProcessor)
  extends BuildFoldingEngine[Params, R, FullR, F, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, R]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens, BuildEngine.validateScenario)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[Params, R]
}

