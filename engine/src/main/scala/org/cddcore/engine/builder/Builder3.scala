package org.cddcore.engine.builder

import scala.reflect.macros.Context
import org.cddcore.engine._
import org.cddcore.utilities._
import scala.language.experimental.macros

object Builder3 {
  def bl[P1, P2, P3, R, FullR]() = new FullBuilderLens[(P1, P2, P3), R, FullR, Builder3[P1, P2, P3, R, FullR]]

  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2, P3) => Boolean]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R, FullR]()
      val ch = CodeHolder[((P1, P2, P3)) => Boolean]((p123) => because.splice(p123._1, p123._2, p123._3), c.literal(show(because.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2, P3) => R]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R, FullR]()
      val ch = CodeHolder[((P1, P2, P3)) => R]((p123) => code.splice(p123._1, p123._2, p123._3), c.literal(show(code.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def assertionImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2, P3) => R]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R, FullR]()
      val ch = CodeHolder[((P1, P2, P3)) => R]((p123) => code.splice(p123._1, p123._2, p123._3), c.literal(show(code.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def matchOnImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2, P3), R]]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      val l = bl[P1, P2, P3, R, FullR]()
      val literal = c.literal(show(pf.tree)).splice
      thisObject.matchOnPrim(pf.splice, literal, literal)
    }
  }
  def assertionFromRImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[(P1, P2, P3, R) => Boolean]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (params: (P1, P2, P3), rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => false
        case Right(r) => assertion.splice(params._1, params._2, params._3, r)
      }
      thisObject.assert(actualAssertion, description)
    }
  }
  def assertionFromPartialFunctionImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[PartialFunction[(P1, P2, P3, R), Boolean]]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (params: (P1, P2, P3), rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => false
        case Right(r) => if (assertion.splice.isDefinedAt(params._1, params._2, params._3, r))
          assertion.splice(params._1, params._2, params._3, r)
        else
          false
      }
      thisObject.assert(actualAssertion, description)
    }
  }

  def assertionFromException[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(assertion: c.Expr[(P1, P2, P3, Exception) => Boolean]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val description = c.literal(show(assertion.tree)).splice
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      val actualAssertion = (params: (P1, P2, P3), rOrE: Either[Exception, R]) => rOrE match {
        case Left(e) => assertion.splice(params._1, params._2, params._3, e)
        case Right(r) => false
      }
      thisObject.assert(actualAssertion, description)
    }
  }
}

case class Builder3[P1, P2, P3, R, FullR](
  nodes: List[BuilderNode[(P1, P2, P3), R]] = List(new EngineDescription[(P1, P2, P3), R]),
  buildExceptions: ExceptionMap = ExceptionMap(),
  buildEngine: BuildEngine[(P1, P2, P3), R, FullR, Engine3[P1, P2, P3, R, FullR]])(implicit val ldp: CddDisplayProcessor)
  extends Builder[(P1, P2, P3), R, FullR, Builder3[P1, P2, P3, R, FullR], Engine3[P1, P2, P3, R, FullR]] {
  def expectedToCode = buildEngine.expectedToCode

  val makeClosures = buildEngine.mc
  val validator = buildEngine.validator
  import bl._
  import makeClosures._
  import validator._

  def scenario(p1: P1, p2: P2, p3: P3, title: String = null) = 
    wrap(s"scenario(${ldp(p1,p2,p3)})", nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => checkDuplicateScenario(bl, this, new Scenario[(P1, P2, P3), R]((p1, p2, p3), Option(title))) :: nodes))

  def because(because: (P1, P2, P3) => Boolean) = macro Builder3.becauseImpl[P1, P2, P3, R, FullR]

  def assert(assertion: (P1, P2, P3, R) => Boolean) = macro Builder3.assertionFromRImpl[P1, P2, P3, R, FullR]
  def assertMatch(assertion: PartialFunction[(P1, P2, P3, R), Boolean]) = macro Builder3.assertionFromPartialFunctionImpl[P1, P2, P3, R, FullR]
  def assertException(assertion: (P1, P2, P3, Exception) => Boolean) = macro Builder3.assertionFromException[P1, P2, P3, R, FullR]

  def code(code: (P1, P2, P3) => R) = macro Builder3.codeImpl[P1, P2, P3, R, FullR]
  def matchOn(pf: PartialFunction[(P1, P2, P3), R]) = macro Builder3.matchOnImpl[P1, P2, P3, R, FullR]
  def matchOnPrim(pf: PartialFunction[(P1, P2, P3), R], becauseToString: String, resultToString: String) = {
    val chBecause = CodeHolder[((P1, P2, P3)) => Boolean]((p123) => pf.isDefinedAt(p123), becauseToString)
    val chResult = CodeHolder[((P1, P2, P3)) => R]((p123) => pf.apply((p123)), resultToString)
    becauseHolder(chBecause).codeHolder(chResult)
  }
  def configurator(cfg: (P1, P2, P3) => Unit) = wrap("configurator", currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ ((params: (P1, P2, P3)) => cfg(params._1, params._2, params._3))))

  def copyNodes(nodes: List[BuilderNode[(P1, P2, P3), R]]) = wrapQuietly(copy(nodes = nodes))
  def build: Engine3[P1, P2, P3, R, FullR] = nodes match {
    case (r: EngineRequirement[(P1, P2, P3), R]) :: nil => buildEngine.buildEngine(r, buildExceptions, ldp)
    case _ => throw new IllegalArgumentException(nodes.toString)
  }
  def copyWithNewExceptions(buildExceptions: ExceptionMap) = wrap("copyWithNewExceptions", copy(buildExceptions = buildExceptions))
}



class FoldingBuildEngine3[P1, P2, P3, R, FullR] extends SimpleFoldingBuildEngine[(P1, P2, P3), R, FullR, Engine3[P1, P2, P3, R, FullR], Engine3[P1, P2, P3, R, R]](
  BuildEngine.defaultRootCode3, new MakeClosures, BuildEngine.expectedToCode3, BuildEngine.builderEngine3[P1, P2, P3, R]) {
  def constructFoldingEngine(
    requirement: EngineRequirement[(P1, P2, P3), R],
    engines: List[EngineFromTests[(P1, P2, P3), R]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR,
    ldp: CddDisplayProcessor): FoldingEngine3[P1, P2, P3, R, FullR] =
    FoldingEngine3(requirement, engines, evaluateTree, exceptionMap, initialValue, foldingFn)(ldp)
}
 class SimpleBuildEngine3[P1, P2, P3, R] extends SimpleBuildEngine[(P1, P2, P3), R, Engine3[P1, P2, P3, R, R]](
  BuildEngine.defaultRootCode3, new MakeClosures, BuildEngine.expectedToCode3) {
  def constructEngine(requirement: EngineRequirement[(P1, P2, P3), R], dt: DecisionTree[(P1, P2, P3), R],
    exceptionMap: ExceptionMap, ldp: CddDisplayProcessor) =
    Engine3FromTests(requirement, dt, evaluateTree, exceptionMap)(ldp)
}

trait Engine3[P1, P2, P3, R, FullR] extends EngineTools[(P1, P2, P3), R] with Function3[P1, P2, P3, FullR] {
  def cached: Function3[P1, P2, P3, FullR] with Engine = new CachedEngine3[P1, P2, P3, R, FullR](this)
}

class CachedEngine3[P1, P2, P3, R, FullR](val delegate: Engine3[P1, P2, P3, R, FullR], val textOrder: Int = Reportable.nextTextOrder) extends Engine3[P1, P2, P3, R, FullR] with CachedEngine[(P1, P2, P3), R, FullR] {
  def apply(p1: P1, p2: P2, p3: P3) = Maps.getOrCreate(cache, (p1, p2, p3), delegate(p1, p2, p3))
}

case class Engine3FromTests[P1, P2, P3, R](
  asRequirement: EngineRequirement[(P1, P2, P3), R],
  tree: DecisionTree[(P1, P2, P3), R],
  evaluator: EvaluateTree[(P1, P2, P3), R],
  buildExceptions: ExceptionMap,
  val textOrder: Int = Reportable.nextTextOrder)(implicit val ldp: CddDisplayProcessor)
  extends Engine3[P1, P2, P3, R, R] with EngineFromTests[(P1, P2, P3), R] {
  def apply(p1: P1, p2: P2, p3: P3) = applyParams(p1, p2, p3)
}
case class FoldingEngine3[P1, P2, P3, R, FullR](
  asRequirement: EngineRequirement[(P1, P2, P3), R],
  engines: List[EngineFromTests[(P1, P2, P3), R]],
  evaluator: EvaluateTree[(P1, P2, P3), R],
  buildExceptions: ExceptionMap,
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR,
  val textOrder: Int = Reportable.nextTextOrder)(implicit val ldp: CddDisplayProcessor)
  extends Engine3[P1, P2, P3, R, FullR]
  with FoldingEngine[(P1, P2, P3), R, FullR] with Function3[P1, P2, P3, FullR] {
  def apply(p1: P1, p2: P2, p3: P3) = applyParams(p1, p2, p3)
}
trait DecisionTreeBuilderForTests3[P1, P2, P3, R] extends DecisionTreeBuilderForTests[(P1, P2, P3), R] {
  def expectedToCode = BuildEngine.expectedToCode3[P1, P2, P3, R]
}

