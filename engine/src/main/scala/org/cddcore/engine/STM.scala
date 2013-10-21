package org.cddcore.engine

//trait StmUniverse[R] extends EngineUniverse[R] {
//
//  trait StmScenarioBuilder extends ScenarioWalker {
//    def useCases: List[UseCase]
//    def withCases(useCases: List[UseCase], fragements: List[F]): RealScenarioBuilder;
//    def thisAsBuilder: RealScenarioBuilder
//    def because(b: Because[B], comment: String = "") = scenarioLens.mod(thisAsBuilder, (s) => s.copy(because = Some(b.copy(comment = comment))))
//    def useCase(description: String) = withCases(UseCase(description, List()) :: useCases, fragments);
//    def expected(e: R) = scenarioLens.mod(thisAsBuilder, (s) => s.copy(expected = Some(e)))
//    def code(c: CodeFn[B, RFn, R], comment: String = "") = scenarioLens.mod(thisAsBuilder, (s) => s.copy(code = Some(c.copy(comment = comment))))
//    def configuration[K](cfg: CfgFn) = scenarioLens.mod(thisAsBuilder, (s) => s.copy(configuration = Some(cfg)))
//    def assertion(a: Assertion[A], comment: String = "") = scenarioLens.mod(thisAsBuilder, (s) => s.copy(assertions = a.copy(comment = comment) :: s.assertions))
//
//    def useCasesForBuild: List[UseCase] =
//      useCases.map(u => UseCase(u.description,
//        u.scenarios.reverse.zipWithIndex.collect {
//          case (s, i) => s.copy(description = Some(s.description.getOrElse(u.description + "[" + i + "]")))
//        })).reverse;
//
//    protected def newScenario(description: String, params: List[Any]) =
//      useCases match {
//        case h :: t => {
//          val descriptionString = if (description == null) None else Some(description);
//          withCases(UseCase(h.description, Scenario(s"${h.description}[${h.scenarios.size}]", descriptionString, params, logger) :: h.scenarios, fragments) :: t)
//        }
//        case _ => throw new NeedUseCaseException
//      }
//  }
//}
//
//class BuilderFactory2Stm[P, R](override val logger: TddLogger = TddLogger.noLogger) extends StmUniverse[R] with Engine2Types[InTxn, P, R] {
//  type RealScenarioBuilder = Builder2Stm
//  def builder = new Builder2Stm
//
//  class Builder2Stm(val useCases: List[UseCase] = List(), val defaultCode: Option[Code] = None) extends ScenarioBuilder {
//    def thisAsBuilder = this
//    def withCases(useCases: List[UseCase], code: Option[Code]) = new Builder2Stm(useCases, code)
//    def scenario(p: P, description: String = null) = newScenario(description, List(p))
//    def build = new Engine(defaultCode) with Function2[InTxn, P, R] {
//      def useCases = useCasesForBuild
//      def apply(inTxn: InTxn, p: P): R = {
//        logParams(p)
//        val rfn: RFn = evaluate((b) => b(inTxn, p), root);
//        val result: R = rfn(inTxn, p)
//        logResult(result)
//      }
//      override def toString() = toStringWithScenarios
//    }
//
//  }
//}
//
//class BuilderFactory3Stm[P1, P2, R](override val logger: TddLogger = TddLogger.noLogger) extends EngineUniverse[R] with Engine3Types[InTxn, P1, P2, R] {
//  type RealScenarioBuilder = Builder3Stm
//  def builder = new Builder3Stm
//  class Builder3Stm(val useCases: List[UseCase] = List(), val defaultCode: Option[Code] = None) extends ScenarioBuilder {
//    def thisAsBuilder = this
//    def withCases(useCases: List[UseCase], code: Option[Code]) = new Builder3Stm(useCases, code)
//    def scenario(p1: P1, p2: P2, description: String = null) = newScenario(description, List(p1, p2))
//    def build = new Engine(defaultCode) with Function2[P1, P2, R] {
//      def useCases = useCasesForBuild
//      def apply(p1: P1, p2: P2): R = atomic { inTxn =>
//        logParams(p1, p2)
//        val rfn: RFn = evaluate((b) => b(inTxn, p1, p2), root);
//        val result: R = rfn(inTxn, p1, p2)
//        logResult(result)
//      }
//      override def toString() = toStringWithScenarios
//    }
//  }
//}