package org.cddcore.legacy

import java.util.concurrent.Future
import java.util.concurrent.atomic.AtomicReference

import org.cddcore.engine._
import org.cddcore.engine.builder._
import org.cddcore.htmlRendering._
import org.cddcore.utilities._
import org.cddcore.utilities.StartChildEndType._

object LegacySamples {
  val replacementEngine = Engine[Int, String]().
    useCase("").
    scenario(1).expected("X").
    scenario(2).expected("XX").because((p: Int) => p == 2).
    build.asInstanceOf[Engine1FromTests[Int, String]];

  val replacementRoot = replacementEngine.tree.root.asDecision
  val conclusionX = replacementRoot.no.asConclusion
  val conclusionXX = replacementRoot.yes.asConclusion
  val conclusionXTitle = ConclusionAsTitle(conclusionX)
  val conclusionXXTitle = ConclusionAsTitle(conclusionXX)

  val categoriserEngine = Engine[LegacyData[Int, Int, String], String]().
    useCase("Pass").expected("pass").
    scenario(LegacyData(1, 1, None, Right("X"), Right("X"))).because((l) => l.expected == l.actual).
    useCase("Fail").expected("fail").
    scenario(LegacyData(1, 1, None, Right("X"), Right("Y"))).because((l) => l.expected != l.actual).
    build.asInstanceOf[Engine1FromTests[LegacyData[Int, Int, String], String]];

  val categoriserRoot = categoriserEngine.tree.root.asDecision
  val passConclusion = categoriserRoot.yes.asConclusion
  val notPassDecision = categoriserRoot.no.asDecision
  val failConclusion = notPassDecision.yes.asConclusion
  val undecidedConclusion = notPassDecision.no.asConclusion
  val passTitle = ConclusionAsTitle(passConclusion)
  val failTitle = ConclusionAsTitle(failConclusion)

  def makeLegacy(ids: Iterable[Int], monitor: LegacyMonitor[Int, Int, String]) =
    new SingleConclusionLegacy[Int, Int, String](ids,
      (x: Int) => x,
      (x: Int) => Right("X".padTo(x, "X").mkString),
      replacementEngine, categoriserEngine,
      monitor)
  def makeLegacyItems(ids: Iterable[Int]) = {
    makeLegacy(ids, monitor)
    monitor.items
  }

  val monitor = new MemoryLegacyMonitor[Int, Int, String]()
  val legacy = makeLegacy(List(1, 2, 3), monitor)
  val legacyReport = LegacyReport("sometitle", legacy, monitor)
  val li123 = monitor.items
  val li1 = li123(0)
  val li2 = li123(1)
  val li3 = li123(2)

}

object LegacyReportRenderer {
  import BuilderPimper._
  import SampleContexts._
  import LegacySamples._

  def itemCount[ID, Params, R](path: List[Reportable], conclusion: AnyConclusion) =
    path.last.asInstanceOf[SingleConclusionLegacyReport[ID, Params, R]].monitor.conclusionToItems(conclusion).size

  val legacyRenderer = Engine[RenderContext, List[Reportable], StartChildEndType, String]().
    title("Legacy Report Renderer").
    renderReport.
    renderFoldingEngines.renderChildEngines.renderEngineFromTests.
    renderDecisionTrees.

    scenario(legacyReport, List(li1, legacyReport), Child).
    expected("" +
      "<div class='legacyItem'><div class='legacyItemTable'>" +
      table("legacyItemTable",
        "legacyTR" -> ("ID" -> "1"),
        "legacyTR" -> ("Description" -> li1.description.getOrElse("")),
        "legacyTR" -> ("Parameter" -> li1.params),
        "legacyTR" -> ("Expected" -> "X"),
        "legacyTR" -> ("Actual" -> "X")) +
        "</div> <!-- legacyItemTable --></div> <!-- legacyItem -->\n").
    matchOn {
      case (rc, path @ (li: AnyLegacyItem) :: tail, Child) =>
        "<div class='legacyItem'><div class='legacyItemTable'>" +
          table("legacyItemTable",
            "legacyTR" -> ("ID" -> li.toId),
            "legacyTR" -> ("Description" -> li.toDescription.getOrElse("")),
            "legacyTR" -> ("Parameter" -> rc.cdp(li.toLegacyItem.params)),
            "legacyTR" -> ("Expected" -> Exceptions.html(li.toExpected)(rc.cdp)),
            "legacyTR" -> ("Actual" -> Exceptions.html(li.toActual)(rc.cdp))) +
            "</div> <!-- legacyItemTable --></div> <!-- legacyItem -->\n"
    }.

    scenario(legacyReport, List(passTitle, li1, legacyReport), Start).
    expected("<div class='conclusionTitle'><h2>pass 2</h2>\n").
    matchOn {
      case (rc, path @ (ct: ConclusionAsTitle) :: tail, Start) =>
        s"<div class='conclusionTitle'><h2>${Strings.htmlEscape(ct.conclusion.toCode.pretty)} ${itemCount(path, ct.conclusion)}</h2>\n"
    }.
    scenario(legacyReport, List(passTitle, li1, legacyReport), End).
    expected("</div> <!-- conclusionTitle -->\n").
    matchOn {
      case (rc, path @ (ct: ConclusionAsTitle) :: tail, End) => "</div> <!-- conclusionTitle -->\n"
    }.
    build
}

object LegacyReport {
  val legacyMonitorKey = "legacyMonitor"
  def defaultItemsToDisplay = 10
  def apply[ID, Params, R](title: String, legacy: Legacy[ID, Params, R, AnySingleConclusionEngine[Params, R]], monitor: MemoryLegacyMonitor[ID, Params, R], conclusion: Option[AnyConclusion] = None, itemstoDisplay: Int = defaultItemsToDisplay): SingleConclusionLegacyReport[ID, Params, R] =
    new SingleConclusionLegacyReport[ID, Params, R](Some(title), legacy, monitor, itemstoDisplay = itemstoDisplay, conclusion = conclusion)
}

case class ConclusionAsTitle(conclusion: AnyConclusion, val textOrder: Int = Reportable.nextTextOrder) extends Reportable {
  override def equals(other: Any) = other match { case ConclusionAsTitle(c, _) => c.equals(conclusion); case _ => false }
  override def hashCode = conclusion.hashCode() + 1
}

case class SingleConclusionLegacyReport[ID, Params, R](title: Option[String], legacy: Legacy[ID, Params, R, AnySingleConclusionEngine[Params, R]], monitor: MemoryLegacyMonitor[ID, Params, R], conclusion: Option[AnyConclusion] = None, description: Option[String] = None, itemstoDisplay: Int = LegacyReport.defaultItemsToDisplay, textOrder: Int = Reportable.nextTextOrder) extends Report {
  private val idToItems = monitor.idToItem
  private val conclusionToItems = monitor.conclusionToItems
  private def items(c: AnyConclusion) = conclusionToItems(c).sortBy(_.id)(Orderings.natural).take(itemstoDisplay)
  private val validItems = conclusion match {
    case Some(c) => conclusionToItems(c).toSet
    case _ => Set[LegacyItem[ID, Params, R]]()
  }
  private val validItemFilter = conclusion match {
    case Some(c) => (li: LegacyItem[ID, Params, R]) => validItems.contains(li)
    case None => (li: LegacyItem[ID, Params, R]) => true
  }
  private val conclusionToTitle = new AtomicReference[Map[List[Reportable], Future[ConclusionAsTitle]]](Map())
  private def getTitle(path: List[Reportable], conclusion: AnyConclusion) = Maps.getOrCreate(conclusionToTitle, path, ConclusionAsTitle(conclusion))

  private def pathFor(e: Engine): List[List[Reportable]] = {
    import EngineTools._
    val ed = e.asRequirement
    val conclusionsInEngine = e.trees.flatMap((x: DecisionTree[_,_])=> x.root.conclusions)
    val conclusions = conclusion match { case Some(c) if conclusionsInEngine.contains(c) => List(c); case _ => e.trees.flatMap((x: DecisionTree[_,_])=> x.root.conclusions) }
    val middle: List[List[Reportable]] = conclusions.flatMap((c) => {
      val h: List[Reportable] = List(getTitle(List(c, ed, this), c), ed, this)
      val m: List[List[Reportable]] = (conclusionToItems.get(c) match {
        case Some(list) => {
          val validItems = list.filter(validItemFilter);
          validItems.sortBy(_.id)(Orderings.natural).take(itemstoDisplay).map((i) => List(i, getTitle(List(c, ed, this), c), ed, this))
        }
        case None => List()
      })
      m match {
        case Nil => Nil
        case _ => h :: m
      }
    })
    val tail = e.trees.flatMap((x: DecisionTree[_,_])=> x.treePathsWithElseClause(List(e.asRequirement, this)))
    List[Reportable](ed, this) :: middle ::: tail
  }
  val reportPaths: List[List[org.cddcore.engine.Reportable]] =
    List(this) ::
      (pathFor(legacy.categorise) :::
        pathFor(legacy.replacement))
  override val urlMapPaths = reportPaths.filter(!_.head.isInstanceOf[AnyLegacyItem])

}


