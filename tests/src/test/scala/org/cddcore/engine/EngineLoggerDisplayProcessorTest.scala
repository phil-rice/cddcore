package org.cddcore.engine
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder._
import org.cddcore.utilities._

@RunWith(classOf[JUnitRunner])
abstract class EngineCddDisplayProcessorTest[Params, R, B <: Builder[Params, R, R, B, E], E <: EngineTools[Params, R]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, R, R, B, E] {
  import EngineTools._
  import ReportableHelper._
  val cdp2 = CddDisplayProcessor(new HtmlCddDisplayer[String](classOf[String]) {
    def plain(cdp: CddDisplayProcessor, s: String) = s"plain{$s}"
    def html(cdp: CddDisplayProcessor, s: String) = s"html{$s}"
  })

  "A builder with a CddDisplayProcessor" should "pass it to the Engine it builds" in {
    resetBuilder(cdp2)
    assert { currentBuilder.ldp.eq(cdp2) }
    val e = build
    assert { e.ldp.eq(cdp2) }
  }


  

}
abstract class EngineCddDisplayProcessor1Test[P, R] extends EngineCddDisplayProcessorTest[P, R,  Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineCddDisplayProcessor2Test[P1, P2, R] extends EngineCddDisplayProcessorTest[(P1, P2),  R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineCddDisplayProcessor3Test[P1, P2, P3, R] extends EngineCddDisplayProcessorTest[(P1, P2, P3),  R,  Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineCddDisplayProcessorStringStringTest extends EngineCddDisplayProcessor1Test[String, String] with StringStringTest {
}

@RunWith(classOf[JUnitRunner])
class EngineCddDisplayProcessorStringStringStringTest extends EngineCddDisplayProcessor2Test[String, String, String] with StringStringStringTest {
}

@RunWith(classOf[JUnitRunner])
class EngineCddDisplayProcessorStringStringStringStringTest extends EngineCddDisplayProcessor3Test[String, String, String, String] with StringStringStringStringTest {
}
