package org.cddcore.engine.builder

import scala.reflect.macros.Context
import org.cddcore.engine._
import org.cddcore.utilities._
import scala.language.experimental.macros
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Future

object Builder1 {
  def bl[P, R, FullR]() = new FullBuilderLens[P, R, FullR, Builder1[P, R, FullR]]

  def becauseImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(because: c.Expr[(P) => Boolean]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P, R, FullR]()
      val ch = CodeHolder[(P) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(code: c.Expr[(P) => R]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P, R, FullR]()
      val ch = CodeHolder[(P) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def assertionFromRImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[(P, R) => Boolean]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (p: P, rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => false
        case Right(r) => assertion.splice(p, r)
      }
      thisObject.assert(actualAssertion, description)
    }
  }
  def assertionFromPartialFunctionImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[PartialFunction[(P, R), Boolean]]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (p: P, rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => false
        case Right(r) => if (assertion.splice.isDefinedAt(p, r)) assertion.splice(p, r) else false
      }
      thisObject.assert(actualAssertion, description)
    }
  }

  def assertionFromException[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[(P, Exception) => Boolean]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (p: P, rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => assertion.splice(p, e)
        case Right(r) => false
      }
      thisObject.assert(actualAssertion, description)
    }
  }
  def matchOnImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P), R]]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      val literal = c.literal(show(pf.tree)).splice
      thisObject.matchOnPrim(pf.splice, literal, literal)
    }
  }
}
case class Builder1[P, R, FullR](
  nodes: List[BuilderNode[P, R]] = List(new EngineDescription[P, R]),
  buildExceptions: ExceptionMap = new ExceptionMap(),
  buildEngine: BuildEngine[P, R, FullR, Engine1[P, R, FullR]])(implicit val ldp: CddDisplayProcessor)

  extends Builder[P, R, FullR, Builder1[P, R, FullR], Engine1[P, R, FullR]] {
  val makeClosures = buildEngine.mc
  val validator = buildEngine.validator
  import bl._
  import makeClosures._
  import validator._
  def expectedToCode = buildEngine.expectedToCode
  def because(because: (P) => Boolean): Builder1[P, R, FullR] = macro Builder1.becauseImpl[P, R, FullR]
  def code(code: (P) => R): Builder1[P, R, FullR] = macro Builder1.codeImpl[P, R, FullR]
  def matchOn(pf: PartialFunction[P, R]) = macro Builder1.matchOnImpl[P, R, FullR]
  def matchOnPrim(pf: PartialFunction[(P), R], becauseToString: String, resultToString: String) = {
    val chBecause = CodeHolder[(P) => Boolean]((p) => pf.isDefinedAt(p), becauseToString)
    val chResult = CodeHolder[(P) => R](p => pf.apply(p), resultToString)
    becauseHolder(chBecause).codeHolder(chResult)
  }
  def scenario(p: P, title: String = null) = wrap(s"scenario(${ldp(p)})", nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) =>
    checkDuplicateScenario(bl, this, new Scenario[P, R](p, title = Option(title))) :: nodes))

  def assert(assertion: (P, R) => Boolean) = macro Builder1.assertionFromRImpl[P, R, FullR]
  def assertMatch(assertion: PartialFunction[(P, R), Boolean]) = macro Builder1.assertionFromPartialFunctionImpl[P, R, FullR]
  def assertException(assertion: (P, Exception) => Boolean) = macro Builder1.assertionFromException[P, R, FullR]

  def configurator(cfg: (P) => Unit) = wrap("configurator", currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ cfg))
  def copyNodes(nodes: List[BuilderNode[P, R]]) = wrapQuietly(copy(nodes = nodes))
  def build: Engine1[P, R, FullR] = nodes match {
    case (r: EngineRequirement[P, R]) :: nil => buildEngine.buildEngine(r, buildExceptions, ldp)
    case _ => throw new IllegalArgumentException(nodes.toString)
  }
  def copyWithNewExceptions(buildExceptions: ExceptionMap) =
    wrap("copyWithNewExceptions", copy(buildExceptions = buildExceptions))
}


 class SimpleBuildEngine1[P, R] extends SimpleBuildEngine[P, R, Engine1[P, R, R]](
  BuildEngine.defaultRootCode1, new MakeClosures, BuildEngine.expectedToCode1) {
  def constructEngine(
    requirement: EngineRequirement[P, R],
    dt: DecisionTree[P, R],
    exceptionMap: ExceptionMap,
    ldp: CddDisplayProcessor): Engine1[P, R, R] =
    Engine1FromTests(requirement, dt, evaluateTree, exceptionMap)(ldp)

}
class FoldingBuildEngine1[P, R, FullR] extends SimpleFoldingBuildEngine[P, R, FullR, Engine1[P, R, FullR], Engine1[P, R, R]](
  BuildEngine.defaultRootCode1, new MakeClosures, BuildEngine.expectedToCode1, BuildEngine.builderEngine1[P, R]) {

  def constructFoldingEngine(
    requirement: EngineRequirement[P, R],
    engines: List[EngineFromTests[P, R]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR,
    ldp: CddDisplayProcessor): FoldingEngine1[P, R, FullR] =
    FoldingEngine1(requirement, engines, evaluateTree, exceptionMap, initialValue, foldingFn)(ldp)
}

trait Engine1[P, R, FullR] extends EngineTools[P, R] with Function1[P, FullR] {
  def cached: Function1[P, FullR] with Engine = new CachedEngine1[P, R, FullR](this)
}

class CachedEngine1[P, R, FullR](val delegate: Engine1[P, R, FullR], val textOrder: Int = Reportable.nextTextOrder) extends Engine1[P, R, FullR] with CachedEngine[P, R, FullR] {
  def apply(p: P) = Maps.getOrCreate(cache, p, delegate(p))
}

case class Engine1FromTests[P, R](
  asRequirement: EngineRequirement[P, R],
  tree: DecisionTree[P, R],
  evaluator: EvaluateTree[P, R],
  buildExceptions: ExceptionMap,
  val textOrder: Int = Reportable.nextTextOrder)(implicit val ldp: CddDisplayProcessor)
  extends Engine1[P, R, R] with EngineFromTests[P, R] with Function1[P, R] {
  def apply(p: P) = applyParams(p)

}
case class FoldingEngine1[P, R, FullR](
  asRequirement: EngineRequirement[P, R],
  engines: List[EngineFromTests[P, R]],
  evaluator: EvaluateTree[P, R],
  buildExceptions: ExceptionMap,
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR,
  val textOrder: Int = Reportable.nextTextOrder)(implicit val ldp: CddDisplayProcessor)
  extends Engine1[P, R, FullR] with FoldingEngine[P, R, FullR] {
  def apply(p: P) = applyParams(p)
}
trait DecisionTreeBuilderForTests1[P, R] extends DecisionTreeBuilderForTests[P, R] {
  def expectedToCode = BuildEngine.expectedToCode1[P, R]
}
