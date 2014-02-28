package org.cddcore.engine

trait Engine1Types[P, R, FullR] extends EngineTypes[R, FullR] {
  type A = (P, ROrException[R]) => Boolean
  type B = (P) => Boolean
  type RFn = (P) => R
  type ParamsTuple = P
  type CfgFn = (P) => Unit
  def rfnMaker = RfnMaker.rfn1ConstantMaker[P, R]
  def makeClosureForBecause(params: ParamsTuple) = BecauseClosure((b: B) => b(params), params)
  def makeClosureForResult(params: ParamsTuple) = (r: RFn) => r(params)
  def makeClosureForCfg(params: List[Any]) = (c: CfgFn) => c(params(0).asInstanceOf[P])
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = (a: A) => a(params(0).asInstanceOf[P], r);
  def tupleToList(p: P) = List(p)
  def listToTuple(params: List[Any]) = params(0).asInstanceOf[P]
}

trait Engine2Types[P1, P2, R, FullR] extends EngineTypes[R, FullR] {
  type A = (P1, P2, ROrException[R]) => Boolean
  type B = (P1, P2) => Boolean
  type RFn = (P1, P2) => R
  type ParamsTuple = (P1, P2)
  type CfgFn = (P1, P2) => Unit
  def rfnMaker = RfnMaker.rfn2ConstantMaker[P1, P2, R]
  def makeClosureForBecause(params: ParamsTuple) = BecauseClosure((b: B) => b(params._1, params._2), params)
  def makeClosureForResult(params: ParamsTuple) = (r: RFn) => r(params._1, params._2)
  def makeClosureForCfg(params: List[Any]) = (c: CfgFn) => c(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = (a: A) => a(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], r);

  def tupleToList(params: (P1, P2)) = List(params._1, params._2)
  def listToTuple(params: List[Any]) = (params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])

}

trait Engine3Types[P1, P2, P3, R, FullR] extends EngineTypes[R, FullR] {
  type A = (P1, P2, P3, ROrException[R]) => Boolean
  type B = (P1, P2, P3) => Boolean
  type RFn = (P1, P2, P3) => R
  type ParamsTuple = (P1, P2, P3)
  type CfgFn = (P1, P2, P3) => Unit
  def rfnMaker = RfnMaker.rfn3ConstantMaker[P1, P2, P3, R]
  def makeClosureForBecause(params: ParamsTuple) = BecauseClosure((b: B) => b(params._1, params._2, params._3), params)
  def makeClosureForResult(params: ParamsTuple) = (r: RFn) => r(params._1, params._2, params._3)
  def makeClosureForCfg(params: List[Any]) = (c: CfgFn) => c(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], params(2).asInstanceOf[P3])
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = (a: A) => a(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], params(2).asInstanceOf[P3], r);
  def tupleToList(params: (P1, P2, P3)) = List(params._1, params._2, params._3)
  def listToTuple(params: List[Any]) = (params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], params(2).asInstanceOf[P3])
}

trait ABuilderFactory1[P, R, FullR] extends EngineUniverse[R, FullR] with Engine1Types[P, R, FullR] {

  trait ABuilder1 extends ScenarioBuilder {
    def scenario(p: P, title: String = null, description: String = null) = newScenario(title, description, (p))
    def build: Engine1[P, FullR] = (builderData.folder, builderData.childEngines.size) match {
      case (Some(_), 0) => throw new CannotHaveFolderWithoutChildEnginesException
      case (None, 0) => new EngineFromTestsImpl(builderData) with Engine1[P, FullR] {
        protected def executeChildEngine(e: ChildEngineDescription, p: P): FullR = {
          throw new IllegalStateException
        }
        def apply(p: P): FullR = applyParams(root, (p), true).asInstanceOf[FullR];
        override def toString() = toStringWithScenarios
      }
      case _ => new EngineWithChildrenImpl(builderData) with EngineFull[R, FullR] with Engine1[P, FullR] {
        def apply(p1: P): FullR = applyParams(List(p1));
        override def toString = "EngineWithChildren(" + children.collect { case e: Engine => e }.map(_.titleString).mkString(",") + ")"
      }
    }
    def ifThen2(pf: PFn) : RealScenarioBuilder = ???
  }
}

class BuilderFactory1[P, R, FullR](folder: Option[(FullR, R) => FullR], initialFoldValue: () => FullR, override val logger: TddLogger = TddLogger.defaultLogger) extends ABuilderFactory1[P, R, FullR] {
  type RealScenarioBuilder = Builder1
  def builder = new Builder1

  class Builder1(val builderData: ScenarioBuilderData = ScenarioBuilderData(logger, arity = 1, folder = folder, initialFoldValue = initialFoldValue)) extends ABuilder1 {
    protected def thisAsBuilder = this
    def copy(builderData: ScenarioBuilderData) = new Builder1(builderData)
  }
}

trait ABuilderFactory2[P1, P2, R, FullR] extends EngineUniverse[R, FullR] with Engine2Types[P1, P2, R, FullR] {

  trait ABuilder2 extends ScenarioBuilder {
    def scenario(p1: P1, p2: P2, title: String = null, description: String = null) = newScenario(title, description, (p1, p2))
    def build: Engine2[P1, P2, FullR] = (builderData.folder, builderData.childEngines.size) match {
      case (Some(_), 0) => throw new CannotHaveFolderWithoutChildEnginesException
      case (None, 0) => new EngineFromTestsImpl(builderData) with Engine2[P1, P2, FullR] {
        def apply(p1: P1, p2: P2): FullR = applyParams(root, (p1, p2), true).asInstanceOf[FullR];
        override def toString() = toStringWithScenarios
      }
      case _ => new EngineWithChildrenImpl(builderData) with EngineFull[R, FullR] with Engine2[P1, P2, FullR] {
        def apply(p1: P1, p2: P2): FullR = applyParams(List(p1, p2));
      }
    }
  }
}

class BuilderFactory2[P1, P2, R, FullR](folder: Option[(FullR, R) => FullR], initialFoldValue: () => FullR, override val logger: TddLogger = TddLogger.defaultLogger) extends ABuilderFactory2[P1, P2, R, FullR] {
  type RealScenarioBuilder = Builder2
  def builder = new Builder2

  class Builder2(val builderData: ScenarioBuilderData = ScenarioBuilderData(logger, arity = 2, folder = folder, initialFoldValue = initialFoldValue)) extends ABuilder2 {
    protected def thisAsBuilder = this
    def copy(builderData: ScenarioBuilderData) = new Builder2(builderData)
  }
}
trait ABuilderFactory3[P1, P2, P3, R, FullR] extends EngineUniverse[R, FullR] with Engine3Types[P1, P2, P3, R, FullR] {

  trait ABuilder3 extends ScenarioBuilder {
    def scenario(p1: P1, p2: P2, p3: P3, title: String = null, description: String = null) = newScenario(title, description, (p1, p2, p3))
    def build: Engine3[P1, P2, P3, FullR] = (builderData.folder, builderData.childEngines.size) match {
      case (Some(_), 0) => throw new CannotHaveFolderWithoutChildEnginesException
      case (None, 0) => new EngineFromTestsImpl(builderData) with Engine3[P1, P2, P3, FullR] {
        def apply(p1: P1, p2: P2, p3: P3): FullR = applyParams(root, (p1, p2, p3), true).asInstanceOf[FullR];
        override def toString() = toStringWithScenarios
      }
      case _ => new EngineWithChildrenImpl(builderData) with EngineFull[R, FullR] with Engine3[P1, P2, P3, FullR] {
        def apply(p1: P1, p2: P2, p3: P3): FullR = applyParams(List(p1, p2, p3));
      }
    }
  }
}

class BuilderFactory3[P1, P2, P3, R, FullR](folder: Option[(FullR, R) => FullR], initialFoldValue: () => FullR, override val logger: TddLogger = TddLogger.defaultLogger) extends ABuilderFactory3[P1, P2, P3, R, FullR] {
  type RealScenarioBuilder = Builder3
  def builder = new Builder3

  class Builder3(val builderData: ScenarioBuilderData = ScenarioBuilderData(logger, arity = 3, folder = folder, initialFoldValue = initialFoldValue)) extends ABuilder3 {
    protected def thisAsBuilder = this
    def copy(builderData: ScenarioBuilderData) = new Builder3(builderData)
  }
}

