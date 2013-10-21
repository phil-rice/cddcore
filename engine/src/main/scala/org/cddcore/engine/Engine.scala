package org.cddcore.engine

class Engine

trait Engine1Types[P, R] extends EngineTypes[R] {
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

trait Engine2Types[P1, P2, R] extends EngineTypes[R] {
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

trait Engine3Types[P1, P2, P3, R] extends EngineTypes[R] {
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



object Engine {
  def apply[P, R]() = new BuilderFactory1[P, R]().builder;
  def apply[P1, P2, R]() = new BuilderFactory2[P1, P2, R]().builder;
  def apply[P1, P2, P3, R]() = new BuilderFactory3[P1, P2, P3, R]().builder;

  def state[S, P, R]() = new BuilderFactory2[S, P, (S, R)]().builder;
  def state[S, P1, P2, R]() = new BuilderFactory3[S, P1, P2, (S, R)]().builder;

  //  def stm[P, R]() = new BuilderFactory2Stm[P, R]().builder;
  //  def stm[P1, P2, R]() = new BuilderFactory3Stm[P1, P2, R]().builder;
}

trait ABuilderFactory1[P, R] extends EngineUniverse[R] with Engine1Types[P, R] {

  trait ABuilder1 extends ScenarioBuilder {
    def scenario(p: P, description: String = null) = newScenario(description, List(p))
    def build = new Engine(description, defaultCode) with Function[P, R] {
      def useCases = useCasesForBuild
      def apply(p: P): R = {
        logParams(p)
        val rfn: RFn = evaluate((b) => b(p), root);
        val result: R = rfn(p)
        logResult(result)
      }
      override def toString() = toStringWithScenarios
    }
  }
}

class BuilderFactory1[P, R](override val logger: TddLogger = TddLogger.noLogger) extends ABuilderFactory1[P, R] {
  type RealScenarioBuilder = Builder1
  def builder = new Builder1

  class Builder1(val description: Option[String] = None, val useCases: List[UseCase] = List(), val defaultCode: Option[Code] = None) extends ABuilder1 {
    def thisAsBuilder = this
    def withCases(description: Option[String], useCases: List[UseCase], code: Option[Code]) = new Builder1(description, useCases, code)
  }
}

trait ABuilderFactory2[P1, P2, R] extends EngineUniverse[R] with Engine2Types[P1, P2, R] {

  trait ABuilder2 extends ScenarioBuilder {
    def scenario(p1: P1, p2: P2, description: String = null) = newScenario(description, List(p1, p2))
    def build = new Engine(description, defaultCode) with Function2[P1, P2, R] {
      def useCases = useCasesForBuild
      def apply(p1: P1, p2: P2): R = {
        logParams(p1, p2)
        val rfn: RFn = evaluate((b) => b(p1, p2), root);
        val result: R = rfn(p1, p2)
        logResult(result)
      }
      override def toString() = toStringWithScenarios
    }
  }
}

class BuilderFactory2[P1, P2, R](override val logger: TddLogger = TddLogger.noLogger) extends ABuilderFactory2[P1, P2, R] {
  type RealScenarioBuilder = Builder2
  def builder = new Builder2

  class Builder2(val description: Option[String] = None, val useCases: List[UseCase] = List(), val defaultCode: Option[Code] = None) extends ABuilder2 {
    def thisAsBuilder = this
    def withCases(description: Option[String], useCases: List[UseCase], code: Option[Code]) = new Builder2(description, useCases, code)
  }
}

class BuilderFactory3[P1, P2, P3, R](override val logger: TddLogger = TddLogger.noLogger) extends EngineUniverse[R] with Engine3Types[P1, P2, P3, R] {

  type RealScenarioBuilder = Builder3
  def builder = new Builder3

  class Builder3(val description: Option[String] = None, val useCases: List[UseCase] = List(), val defaultCode: Option[Code] = None) extends ScenarioBuilder {
    def thisAsBuilder = this
    def withCases(engineDescription: Option[String], useCases: List[UseCase], code: Option[Code]) = new Builder3(engineDescription, useCases, code)
    def scenario(p1: P1, p2: P2, p3: P3, description: String = null) = newScenario(description, List(p1, p2, p3))
    def build = new Engine(description, defaultCode) with Function3[P1, P2, P3, R] {
      def useCases = useCasesForBuild
      def apply(p1: P1, p2: P2, p3: P3): R = {
        logParams(p1, p2)
        val rfn: RFn = evaluate((b) => b(p1, p2, p3), root);
        val result: R = rfn(p1, p2, p3)
        logResult(result)
      }
      override def toString() = toStringWithScenarios
    }
  }
}