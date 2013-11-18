package org.cddcore.engine

trait Engine1Types[P, R, FullR] extends EngineTypes[R, FullR] {
  type A = (P, ROrException[R]) => Boolean
  type B = (P) => Boolean
  type RFn = (P) => R
  type CfgFn = (P) => Unit
  def rfnMaker = RfnMaker.rfn1ConstantMaker[P, R]
  def makeClosureForBecause(params: List[Any]) = (b: B) => b(params(0).asInstanceOf[P])
  def makeClosureForResult(params: List[Any]) = (r: RFn) => r(params(0).asInstanceOf[P])
  def makeClosureForCfg(params: List[Any]) = (c: CfgFn) => c(params(0).asInstanceOf[P])
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = (a: A) => a(params(0).asInstanceOf[P], r);
}

trait Engine2Types[P1, P2, R, FullR] extends EngineTypes[R, FullR] {
  type A = (P1, P2, ROrException[R]) => Boolean
  type B = (P1, P2) => Boolean
  type RFn = (P1, P2) => R
  type CfgFn = (P1, P2) => Unit
  def rfnMaker = RfnMaker.rfn2ConstantMaker[P1, P2, R]
  def makeClosureForBecause(params: List[Any]) = (b: B) => b(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])
  def makeClosureForResult(params: List[Any]) = (r: RFn) => r(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])
  def makeClosureForCfg(params: List[Any]) = (c: CfgFn) => c(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2])
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = (a: A) => a(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], r);
}

trait Engine3Types[P1, P2, P3, R, FullR] extends EngineTypes[R, FullR] {
  type A = (P1, P2, P3, ROrException[R]) => Boolean
  type B = (P1, P2, P3) => Boolean
  type RFn = (P1, P2, P3) => R
  type CfgFn = (P1, P2, P3) => Unit
  def rfnMaker = RfnMaker.rfn3ConstantMaker[P1, P2, P3, R]
  def makeClosureForBecause(params: List[Any]) = (b: B) => b(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], params(2).asInstanceOf[P3])
  def makeClosureForResult(params: List[Any]) = (r: RFn) => r(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], params(2).asInstanceOf[P3])
  def makeClosureForCfg(params: List[Any]) = (c: CfgFn) => c(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], params(2).asInstanceOf[P3])
  def makeClosureForAssertion(params: List[Any], r: ROrException[R]) = (a: A) => a(params(0).asInstanceOf[P1], params(1).asInstanceOf[P2], params(2).asInstanceOf[P3], r);
}

trait ABuilderFactory1[P, R, FullR] extends EngineUniverse[R, FullR] with Engine1Types[P, R, FullR] {

  trait ABuilder1 extends ScenarioBuilder {
    def scenario(p: P, title: String = null, description: String = null) = newScenario(title, description, List(p))
    def build = new Engine(builderData) with Engine1[P, R] {
      protected def executeChildEngine(e: ChildEngineDescription, p: P): FullR = {
        throw new IllegalStateException
      }
      def apply(p: P): R = applyParams(root, List(p), true);
      override def toString() = toStringWithScenarios
    }
  }
}

class BuilderFactory1[P, R, FullR](override val logger: TddLogger = TddLogger.defaultLogger) extends ABuilderFactory1[P, R, FullR] {
  type RealScenarioBuilder = Builder1
  def builder = new Builder1

  class Builder1(val builderData: ScenarioBuilderData = ScenarioBuilderData()) extends ABuilder1 {
    protected def thisAsBuilder = this
    def copy(builderData: ScenarioBuilderData) = new Builder1(builderData)
  }
}

trait ABuilderFactory2[P1, P2, R, FullR] extends EngineUniverse[R, FullR] with Engine2Types[P1, P2, R, FullR] {

  trait ABuilder2 extends ScenarioBuilder {
    def scenario(p1: P1, p2: P2, title: String = null, description: String = null) = newScenario(title, description, List(p1, p2))
    def build = new Engine(builderData) with Engine2[P1, P2, R] {
      def apply(p1: P1, p2: P2): R = applyParams(root, List(p1, p2), true);
      override def toString() = toStringWithScenarios
    }
  }
}

class BuilderFactory2[P1, P2, R, FullR](override val logger: TddLogger = TddLogger.defaultLogger) extends ABuilderFactory2[P1, P2, R, FullR] {
  type RealScenarioBuilder = Builder2
  def builder = new Builder2

  class Builder2(val builderData: ScenarioBuilderData = ScenarioBuilderData()) extends ABuilder2 {
    protected def thisAsBuilder = this
    def copy(builderData: ScenarioBuilderData) = new Builder2(builderData)
  }
}

class BuilderFactory3[P1, P2, P3, R, FullR](override val logger: TddLogger = TddLogger.defaultLogger) extends EngineUniverse[R, FullR] with Engine3Types[P1, P2, P3, R, FullR] {

  type RealScenarioBuilder = Builder3
  def builder = new Builder3

  class Builder3(val builderData: ScenarioBuilderData = ScenarioBuilderData()) extends ScenarioBuilder {
    protected def thisAsBuilder = this
    def copy(builderData: ScenarioBuilderData) = new Builder3(builderData)
    def scenario(p1: P1, p2: P2, p3: P3, title: String = null, description: String = null) = newScenario(title, description, List(p1, p2, p3))
    def build = new Engine(builderData) with Engine3[P1, P2, P3, R] {

      def apply(p1: P1, p2: P2, p3: P3): R = applyParams(root, List(p1, p2, p3), true);
      override def toString() = toStringWithScenarios
    }
  }
}