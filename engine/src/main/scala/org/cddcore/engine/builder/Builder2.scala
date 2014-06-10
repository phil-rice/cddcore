package org.cddcore.engine.builder

import scala.reflect.macros.Context
import org.cddcore.engine._
import org.cddcore.utilities._
import scala.language.experimental.macros

object Builder2 {
  def bl[P1, P2, R, FullR]() = new FullBuilderLens[(P1, P2), R, FullR, Builder2[P1, P2, R, FullR]]
  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2) => Boolean]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R, FullR]()
      val ch = CodeHolder[((P1, P2)) => Boolean]((p12) => because.splice(p12._1, p12._2), c.literal(show(because.tree)).splice)
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2) => R]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R, FullR]()
      val ch = CodeHolder[((P1, P2)) => R]((p12) => code.splice(p12._1, p12._2), c.literal(show(code.tree)).splice)
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def matchOnImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2), R]]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      val literal = c.literal(show(pf.tree)).splice
      thisObject.matchOnPrim(pf.splice, literal, literal)
    }
  }
  def assertionFromRImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[(P1, P2, R) => Boolean]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (params: (P1, P2), rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => false
        case Right(r) => assertion.splice(params._1, params._2, r)
      }
      thisObject.assert(actualAssertion, description)
    }
  }
  def assertionFromPartialFunctionImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[PartialFunction[(P1, P2, R), Boolean]]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (params: (P1, P2), rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => false
        case Right(r) => if (assertion.splice.isDefinedAt(params._1, params._2, r)) assertion.splice(params._1, params._2, r) else false
      }
      thisObject.assert(actualAssertion, description)
    }
  }

  def assertionFromException[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[(P1, P2, Exception) => Boolean]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (params: (P1, P2), rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => assertion.splice(params._1, params._2, e)
        case Right(r) => false
      }

      thisObject.assert(actualAssertion, description)
    }
  }
}

case class Builder2[P1, P2, R, FullR](
  nodes: List[BuilderNode[(P1, P2), R]] = List(new EngineDescription[(P1, P2), R]),
  buildExceptions: ExceptionMap = new ExceptionMap(),
  buildEngine: BuildEngine[(P1, P2), R, FullR, Engine2[P1, P2, R, FullR]])(implicit val ldp: CddDisplayProcessor)
  extends Builder[(P1, P2), R, FullR, Builder2[P1, P2, R, FullR], Engine2[P1, P2, R, FullR]] {
  def expectedToCode = buildEngine.expectedToCode

  val makeClosures = buildEngine.mc
  val validator = buildEngine.validator
  import bl._
  import makeClosures._
  import validator._

  def code(code: (P1, P2) => R): Builder2[P1, P2, R, FullR] = macro Builder2.codeImpl[P1, P2, R, FullR]
  def because(because: (P1, P2) => Boolean): Builder2[P1, P2, R, FullR] = macro Builder2.becauseImpl[P1, P2, R, FullR]
  def matchOn(pf: PartialFunction[(P1, P2), R]) = macro Builder2.matchOnImpl[P1, P2, R, FullR]
  def matchOnPrim(pf: PartialFunction[(P1, P2), R], becauseToString: String, resultToString: String) = {
    val chBecause = CodeHolder[((P1, P2)) => Boolean]((p12) => pf.isDefinedAt(p12), becauseToString)
    val chResult = CodeHolder[((P1, P2)) => R]((p12) => pf.apply(p12), resultToString)
    becauseHolder(chBecause).codeHolder(chResult)
  }
  def scenario(p1: P1, p2: P2, title: String = null): Builder2[P1, P2, R, FullR] =
    wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => checkDuplicateScenario(bl, this, new Scenario[(P1, P2), R]((p1, p2), title = Option(title))) :: nodes))

  def assert(assertion: (P1, P2, R) => Boolean) = macro Builder2.assertionFromRImpl[P1, P2, R, FullR]
  def assertMatch(assertion: PartialFunction[(P1, P2, R), Boolean]) = macro Builder2.assertionFromPartialFunctionImpl[P1, P2, R, FullR]
  def assertException(assertion: (P1, P2, Exception) => Boolean) = macro Builder2.assertionFromException[P1, P2, R, FullR]

  def configurator(cfg: (P1, P2) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, (l) => l :+ ((params: (P1, P2)) => cfg(params._1, params._2))))
  def copyNodes(nodes: List[BuilderNode[(P1, P2), R]]) = wrap(copy(nodes = nodes))
  def build: Engine2[P1, P2, R, FullR] = nodes match {
    case (r: EngineRequirement[(P1, P2), R]) :: nil => buildEngine.buildEngine(r, buildExceptions, ldp)
    case _ => throw new IllegalArgumentException(nodes.toString)
  }
  def copyWithNewExceptions(buildExceptions: ExceptionMap) = wrap(copy(buildExceptions = buildExceptions))
}


class FoldingBuildEngine2[P1, P2, R, FullR] extends SimpleFoldingBuildEngine[(P1, P2), R, FullR, Engine2[P1, P2, R, FullR], Engine2[P1, P2, R, R]](
  BuildEngine.defaultRootCode2, new MakeClosures, BuildEngine.expectedToCode2, BuildEngine.builderEngine2[P1, P2, R]) {
  def constructFoldingEngine(
    requirement: EngineRequirement[(P1, P2), R],
    engines: List[EngineFromTests[(P1, P2), R]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR,
    ldp: CddDisplayProcessor): FoldingEngine2[P1, P2, R, FullR] =
    FoldingEngine2(requirement, engines, evaluateTree, exceptionMap, initialValue, foldingFn)(ldp)
}
case class SimpleBuildEngine2[P1, P2, R] extends SimpleBuildEngine[(P1, P2), R, Engine2[P1, P2, R, R]](
  BuildEngine.defaultRootCode2, new MakeClosures, BuildEngine.expectedToCode2) {
  def constructEngine(
    requirement: EngineRequirement[(P1, P2), R],
    dt: DecisionTree[(P1, P2), R],
    exceptionMap: ExceptionMap,
    ldp: CddDisplayProcessor) =
    Engine2FromTests(requirement, dt, evaluateTree, exceptionMap)(ldp)
}

trait Engine2[P1, P2, R, FullR] extends EngineTools[(P1, P2), R] with Function2[P1, P2, FullR] {
  def cached: Function2[P1, P2, FullR] with Engine = new CachedEngine2[P1, P2, R, FullR](this)
}

class CachedEngine2[P1, P2, R, FullR](val delegate: Engine2[P1, P2, R, FullR], val textOrder: Int = Reportable.nextTextOrder) extends Engine2[P1, P2, R, FullR] with CachedEngine[(P1, P2), R, FullR] {
  def apply(p1: P1, p2: P2) = Maps.getOrCreate(cache, (p1, p2), delegate(p1, p2))
}

case class Engine2FromTests[P1, P2, R](
  asRequirement: EngineRequirement[(P1, P2), R],
  tree: DecisionTree[(P1, P2), R],
  evaluator: EvaluateTree[(P1, P2), R],
  buildExceptions: ExceptionMap,
  val textOrder: Int = Reportable.nextTextOrder)(implicit val ldp: CddDisplayProcessor)
  extends Engine2[P1, P2, R, R] with EngineFromTests[(P1, P2), R] with Function2[P1, P2, R] {
  def apply(p1: P1, p2: P2) = applyParams((p1, p2))
}

case class FoldingEngine2[P1, P2, R, FullR](
  asRequirement: EngineRequirement[(P1, P2), R],
  engines: List[EngineFromTests[(P1, P2), R]],
  evaluator: EvaluateTree[(P1, P2), R],
  buildExceptions: ExceptionMap,
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR,
  val textOrder: Int = Reportable.nextTextOrder)(implicit val ldp: CddDisplayProcessor)
  extends Engine2[P1, P2, R, FullR] with FoldingEngine[(P1, P2), R, FullR] {
  def apply(p1: P1, p2: P2) = applyParams(p1, p2)
}
trait DecisionTreeBuilderForTests2[P1, P2, R] extends DecisionTreeBuilderForTests[(P1, P2), R] {
  def expectedToCode = BuildEngine.expectedToCode2[P1, P2, R]
}
