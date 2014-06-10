package org.cddcore.engine

import org.cddcore.engine.builder.Conclusion
import org.cddcore.engine.builder.FoldingEngine1
import org.cddcore.utilities.CddDisplayProcessor
import org.cddcore.utilities.TraceItem
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder.AnyConclusion

@RunWith(classOf[JUnitRunner])
class EngineMonitorTest extends AbstractTest {

  val times2 = Engine[Int, Int].title("times two").scenario(2).expected(4).code(_ * 2).build
  val plus2 = Engine[Int, Int].title("plus two").scenario(2).expected(4).code(_ + 2).build
  val times2plus2 = Engine[Int, Int].title("times two plus two").scenario(2).expected(6).code(times2(_) + 2).build
  val blank = Engine[Int, Int].title("Blank").build

  class EngineMonitorMemory extends EngineMonitor {
    private var list = List[String]()
    def call[Params](e: Engine, params: Params)(implicit ldp: CddDisplayProcessor) =
      list = s"call ${e.titleString}" :: list
    def finished[R](e: Engine, conclusion: Option[AnyConclusion], result: R)(implicit ldp: CddDisplayProcessor) =
      list = s"finished ${e.titleString}" :: list
    def failed(e: Engine, conclusion: Option[AnyConclusion], exception: Exception)(implicit ldp: CddDisplayProcessor) =
      list = s"failed ${e.titleString}" :: list
    def memory = list.reverse
  }
  "The Engine With Monitor method" should "report calls then finished and failures" in {
    val m = new EngineMonitorMemory
    assertEquals(4, Engine.withMonitor(m, {
      evaluating { blank(0) } should produce[UndecidedException]
      plus2(2)
    }))
    assertEquals(List("call Blank", "failed Blank", "call plus two", "finished plus two"), m.memory)

  }

  it should "allow nested withMonitor calls" in {
    val m = new EngineMonitorMemory
    val m1 = new EngineMonitorMemory
    val m2 = new EngineMonitorMemory
    assertEquals(4, Engine.withMonitor(m, {
      Engine.withMonitor(m1, evaluating { blank(0) } should produce[UndecidedException])
      Engine.withMonitor(m2, plus2(2))
    }))
    assertEquals(List("call Blank", "failed Blank"), m1.memory)
    assertEquals(List( "call plus two", "finished plus two"), m2.memory)
    assertEquals(List("call Blank", "failed Blank", "call plus two", "finished plus two"), m.memory)

  }

  "The Engine Trace method" should "return trace items representing the engine calls that took place" in {

    val (result, trace) = Engine.traceNoException(times2(plus2(2)))
    assertEquals(8, result)
    assertEquals(List(TraceItem(plus2, 2, Right(4), None, List(), 0), TraceItem(times2, 4, Right(8), None, List(), 0)), trace)
  }

  it should "return trace items for nested calls" in {

    val (result, trace) = Engine.traceNoException(times2plus2(2))
    assertEquals(6, result)
    type TI = TraceItem[Engine, Any, Any, Any]
    val innerTI: TI = TraceItem(times2, 2, Right(4), None, List(), 0)
    val outerTI: TI = TraceItem(times2plus2, 2, Right(6), None, List(innerTI), 0): TI
    assertEquals(List(outerTI), trace)
  }
  it should "return trace items for folded calls" in {
    val e = Engine.foldList[Int, Int].title("times two and plus two").
      childEngine("times two").scenario(2).expected(4).code(_ * 2).
      childEngine("plus two").scenario(2).expected(4).code(_ * 2).
      build
    val f = e.asInstanceOf[FoldingEngine1[Int, Int, List[Int]]]
    val ce0 = f.engines(0)
    val ce1 = f.engines(1)

    val (result, trace) = Engine.traceNoException(e(2))
    assertEquals(List(4, 4), result)
    type TI = TraceItem[Engine, Any, Any, Any]
    val timesTwoTi: TI = TraceItem(ce0, 2, Right(4), None, List(), 0)
    val plusTwoTi: TI = TraceItem(ce1, 2, Right(4), None, List(), 0): TI
    val outerTi: TI = TraceItem(f, 2, Right(List(4, 4)), None, List(timesTwoTi, plusTwoTi), 0): TI
    assertEquals(List(outerTi), trace)
  }
}