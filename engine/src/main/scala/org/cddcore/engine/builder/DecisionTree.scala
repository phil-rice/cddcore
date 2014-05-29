package org.cddcore.engine.builder

import scala.language.implicitConversions
import org.cddcore.utilities.Lens
import org.cddcore.utilities.CodeHolder
import org.cddcore.engine._
import org.cddcore.utilities.NestedHolder

class DecisionTreeLens[Params, BFn, R, RFn](val creator: (DecisionTreeNode[Params, BFn, R, RFn]) => DecisionTree[Params, BFn, R, RFn] = (root: DecisionTreeNode[Params, BFn, R, RFn]) => new SimpleDecisionTree(root, false)) {
  val rootL = Lens[DecisionTree[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (tree) => tree.root,
    (tree, root) => creator(root),
    Some("rootL"))

  //  def exceptionMapL(asRequirement: BuilderNodeAndHolder[Params, BFn,R, RFn] ) =
  //    Lens[DecisionTree[Params, BFn, R, RFn],ExceptionMap](
  //      (tree) => tree.buildExceptions,
  //      (tree, buildExceptions) => creator(asRequirement)(tree.root, buildExceptions),
  //      Some("exceptionMapL"))

  def toDecisionL = Lens[DecisionTreeNode[Params, BFn, R, RFn], Decision[Params, BFn, R, RFn]](
    (d) => d.asInstanceOf[Decision[Params, BFn, R, RFn]],
    (d, x) => x,
    Some("toDecisionL"))
  def toConclusionL = Lens[DecisionTreeNode[Params, BFn, R, RFn], Conclusion[Params, BFn, R, RFn]](
    (d) => d.asInstanceOf[Conclusion[Params, BFn, R, RFn]],
    (d, x) => x,
    Some("toConclusionL"))
  def yesL = Lens[Decision[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (d) => d.yes,
    (d, x) => d.copy(yes = x),
    Some("yesL"))
  def noL = Lens[Decision[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (d) => d.no,
    (d, x) => d.copy(no = x),
    Some("noL"))
}
object ElseClause {
  private val instance = new ElseClause
  def apply() = instance
}

class ElseClause(val textOrder: Int = Reportable.nextTextOrder) extends ReportableWithoutUrl

trait DecisionTree[Params, BFn, R, RFn] extends NestedHolder[DecisionTreeNode[Params, BFn, R, RFn]] with Reportable {
  def root: DecisionTreeNode[Params, BFn, R, RFn]
  def rootIsDefault: Boolean
  def nodes = List(root)
  def treePathsWithElseClause[Params, BFn, R, RFn](pathNotIncludingTree: List[Reportable]): List[List[Reportable]] =
    (this :: pathNotIncludingTree) :: treePaths(this :: pathNotIncludingTree, root)

  //builds up in reverse order
  private def treePaths[Params, BFn, R, RFn](path: List[Reportable], node: DecisionTreeNode[Params, BFn, R, RFn]): List[List[Reportable]] = {
    node match {
      case c: Conclusion[Params, BFn, R, RFn] => List((c :: path))
      case d: Decision[Params, BFn, R, RFn] => {
        val dPath = (d :: path)
        val yesPaths = treePaths(dPath, d.yes)
        val noPaths = treePaths(dPath, d.no)
        dPath :: yesPaths ::: List(ElseClause() :: dPath) ::: noPaths
      }
    }
  }
}

case class SimpleDecisionTree[Params, BFn, R, RFn](root: DecisionTreeNode[Params, BFn, R, RFn], val rootIsDefault: Boolean = true, textOrder: Int = Reportable.nextTextOrder) extends DecisionTree[Params, BFn, R, RFn] {
  override def hashCode = root.hashCode()
  override def equals(other: Any) = other match {
    case sdt: SimpleDecisionTree[Params, BFn, R, RFn] => root == sdt.root && rootIsDefault == sdt.rootIsDefault
    case _ => false
  }
}
trait AnyDecisionTreeNode extends Reportable {
  def toDecisionTreeNode[Params, BFn, R, RFn] = this.asInstanceOf[DecisionTreeNode[Params, BFn, R, RFn]]
  def containsConclusion(c: AnyConclusion): Boolean
}

sealed trait DecisionTreeNode[Params, BFn, R, RFn] extends Reportable with AnyDecisionTreeNode {
  def scenarios: List[Scenario[Params, BFn, R, RFn]]
  def asConclusion = asInstanceOf[Conclusion[Params, BFn, R, RFn]]
  def asDecision = asInstanceOf[Decision[Params, BFn, R, RFn]]
}

trait AnyConclusion extends AnyDecisionTreeNode {
  def toConclusion[Params, BFn, R, RFn] = this.asInstanceOf[Conclusion[Params, BFn, R, RFn]]
}

case class Conclusion[Params, BFn, R, RFn](code: CodeHolder[RFn], scenarios: List[Scenario[Params, BFn, R, RFn]], textOrder: Int = Reportable.nextTextOrder) extends DecisionTreeNode[Params, BFn, R, RFn] with AnyConclusion {
  def containsConclusion(c: AnyConclusion) = eq(c)
  override def hashCode = textOrder
  override def equals(other: Any) = other match {
    case c: Conclusion[Params, BFn, R, RFn] => c.code == code && c.scenarios == scenarios
    case _ => false
  }
}

trait AnyDecision extends AnyDecisionTreeNode {
  def toDecision[Params, BFn, R, RFn] = this.asInstanceOf[Decision[Params, BFn, R, RFn]]
  def toYes[Params, BFn, R, RFn] = toDecision[Params, BFn, R, RFn].yes
  def toNo[Params, BFn, R, RFn] = toDecision[Params, BFn, R, RFn].no
  def toPrettyString[Params, BFn, R, RFn] = toDecision[Params, BFn, R, RFn].prettyString
}

case class Decision[Params, BFn, R, RFn](because: List[CodeHolder[BFn]], yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn], scenarioThatCausedNode: Scenario[Params, BFn, R, RFn], textOrder: Int = Reportable.nextTextOrder)
  extends DecisionTreeNode[Params, BFn, R, RFn] with NestedHolder[DecisionTreeNode[Params, BFn, R, RFn]] with AnyDecision {
  def containsConclusion(c: AnyConclusion) = yes.containsConclusion(c) || no.containsConclusion(c)
  def scenarios = yes.scenarios ++ no.scenarios
  def isTrue(bc: (BFn) => Boolean) = because.foldLeft(false)((acc, ch) => acc || bc(ch.fn))
  def prettyString = because.map(_.pretty).mkString(" or ")
  def nodes = List(yes, no)
  override lazy val hashCode = (because.hashCode + yes.hashCode() + no.hashCode()) / 3
  override def equals(other: Any) = other match {
    case d: Decision[Params, BFn, R, RFn] => d.because == because && d.yes == yes && d.no == no
    case _ => false
  }
}

trait MakeClosures[Params, BFn, R, RFn] {
  type S = Scenario[Params, BFn, R, RFn]
  type Result = Either[Exception, R]
  type BecauseClosure = (BFn) => Boolean
  type ResultClosure = (RFn) => R

  def safe[T](block: => T): Either[Exception, T] = try Right(block) catch { case e: Exception => Left(e) }

  def makeBecauseClosure(params: Params): BecauseClosure
  def makeBecauseClosure(scenarioWithParams: Scenario[Params, BFn, R, RFn]): BecauseClosure

  protected def wrapBecause[X](s: S, x: => X): X = try { x } catch { case e: Exception => throw BecauseClauseScenarioException(s, e) }
  protected def wrapBecause[X](p: Params, x: => X): X = try { x } catch { case e: Exception => throw BecauseClauseException(p, e) }

  def makeResultClosure(params: Params): ResultClosure
  def makeResultClosure(scenarioWithParams: Scenario[Params, BFn, R, RFn]): ResultClosure

  def evaluateBecause(scenarioWithbecause: Scenario[Params, BFn, R, RFn], scenarioWithParams: Scenario[Params, BFn, R, RFn]): Boolean =
    makeBecauseClosure(scenarioWithParams)(scenarioWithbecause.
      because.getOrElse(throw new IllegalArgumentException(s"Scenario doesn't have because\scenarioWithbecause")).fn)
  def evaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, BFn, R, RFn]) = makeResultClosure(scenarioWithParams)(rfn)
  def safeEvaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, BFn, R, RFn]) = safe(makeResultClosure(scenarioWithParams)(rfn))
}

case class SimpleEvaluateTree[Params, BFn, R, RFn](
  val makeClosures: MakeClosures[Params, BFn, R, RFn],
  val lens: DecisionTreeLens[Params, BFn, R, RFn],
  val validator: ValidateScenario[Params, BFn, R, RFn]) extends EvaluateTree[Params, BFn, R, RFn] {
}

trait EvaluateTree[Params, BFn, R, RFn] {
  type DT = DecisionTree[Params, BFn, R, RFn]
  type DTN = DecisionTreeNode[Params, BFn, R, RFn]

  val makeClosures: MakeClosures[Params, BFn, R, RFn]
  val validator: ValidateScenario[Params, BFn, R, RFn]
  val lens: DecisionTreeLens[Params, BFn, R, RFn]
  import makeClosures._
  import lens._

  def findLensToConclusion(root: DTN, s: S): Lens[DT, DTN] = findLensToConclusion(root, makeBecauseClosure(s), rootL)
  def findLensToConclusion(root: DTN, bc: (BFn) => Boolean): Lens[DT, DTN] = findLensToConclusion(root, bc, rootL)
  def findLensToLastDecisionNode(root: DTN, s: S): Option[Lens[DT, Decision[Params, BFn, R, RFn]]] = findLensToLastDecisionNode(root, makeBecauseClosure(s), rootL.andThen(toDecisionL))

  def findPathToConclusionWithParams(tree: DT, params: Params): List[DTN] = findPathToConclusionPrim(tree.root, makeBecauseClosure(params), List())

  def findPathToConclusionWithConclusion(root: AnyDecisionTreeNode, conclusion: AnyConclusion, path: List[AnyDecisionTreeNode]): List[AnyDecisionTreeNode] = {
    root match {
      case c: AnyConclusion if c.containsConclusion(conclusion) => c :: path
      case d: AnyDecision if d.toYes.containsConclusion(conclusion) => findPathToConclusionWithConclusion(d.toYes, conclusion, d :: path)
      case d: AnyDecision if d.toNo.containsConclusion(conclusion) => findPathToConclusionWithConclusion(d.toNo, conclusion, d :: path)
    }
  }
  protected def findPathToConclusionPrim(root: DTN, becauseClosure: BecauseClosure, path: List[DTN]): List[DTN] = {

    root match {
      case d: Decision[Params, BFn, R, RFn] => d.isTrue(becauseClosure) match {
        case true => findPathToConclusionPrim(d.yes, becauseClosure, d :: path)
        case false => findPathToConclusionPrim(d.no, becauseClosure, d :: path)
      }
      case c: Conclusion[Params, BFn, R, RFn] => c :: path
    }
  }
  def safeEvaluate(tree: DT, scenarioWithParams: S) = safe(evaluate(tree, scenarioWithParams))

  def evaluate(tree: DT, scenarioWithParams: S): R = {
    val bc = makeBecauseClosure(scenarioWithParams)
    val c = findConclusion(tree, bc).code
    makeResultClosure(scenarioWithParams).apply(c.fn)
  }
  def evaluate(tree: DT, params: Params): R = {
    val bc = makeBecauseClosure(params)
    val c = findConclusion(tree, bc).code
    makeResultClosure(params).apply(c.fn)
  }

  def findConclusion(tree: DT, bc: BecauseClosure): Conclusion[Params, BFn, R, RFn] = findConclusion(tree.root, bc)

  def findConclusion(root: DTN, bc: BecauseClosure): Conclusion[Params, BFn, R, RFn] = {
    root match {
      case c: Conclusion[Params, BFn, R, RFn] => c
      case d: Decision[Params, BFn, R, RFn] => d.isTrue(bc) match {
        case true => findConclusion(d.yes, bc)
        case false => findConclusion(d.no, bc)
      }
    }
  }

  type LensToNode = Lens[DecisionTree[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]]

  protected def findLensToConclusion(root: DTN, bc: BecauseClosure, soFar: LensToNode): Lens[DT, DTN] =
    root match {
      case c: Conclusion[Params, BFn, R, RFn] => soFar
      case d: Decision[Params, BFn, R, RFn] => d.isTrue(bc) match {
        case true => findLensToConclusion(d.yes, bc, soFar.andThen(toDecisionL).andThen(yesL))
        case false => findLensToConclusion(d.no, bc, soFar.andThen(toDecisionL).andThen(noL))
      }
    }
  protected def findLensToLastDecisionNode(root: DTN, bc: BecauseClosure, soFar: Lens[DT, Decision[Params, BFn, R, RFn]]): Option[Lens[DT, Decision[Params, BFn, R, RFn]]] =
    root match {
      case d: Decision[Params, BFn, R, RFn] => {
        def returnMeOrRecurse(child: DTN, nextSoFar: LensToNode): Option[Lens[DT, Decision[Params, BFn, R, RFn]]] =
          child match {
            case c: Conclusion[Params, BFn, R, RFn] => Some(soFar)
            case d: Decision[Params, BFn, R, RFn] => findLensToLastDecisionNode(child, bc, nextSoFar.andThen(toDecisionL))
          }
        d.isTrue(bc) match {
          case true => returnMeOrRecurse(d.yes, soFar.andThen(yesL))
          case false => returnMeOrRecurse(d.no, soFar.andThen(noL))
        }
      }
      case c: Conclusion[Params, BFn, R, RFn] => None
    }
}

trait DecisionTreeBuilderForTests[Params, BFn, R, RFn] {
  def expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]
  def scen(params: Params, title: String = null, description: Option[String] = None, because: Option[CodeHolder[BFn]] = None,
    code: Option[CodeHolder[RFn]] = None, priority: Option[Int] = None, expected: Option[Either[Exception, R]] = None,
    references: Set[Reference] = Set()) =
    new Scenario[Params, BFn, R, RFn](params, Option(title), description, because, code, priority, expected, references)
  def uc(title: String = null) = new UseCase[Params, BFn, R, RFn](Option(title))
  def conc(scenario: Scenario[Params, BFn, R, RFn], scenarios: Scenario[Params, BFn, R, RFn]*) =
    new Conclusion[Params, BFn, R, RFn](scenario.actualCode(expectedToCode), List(scenario) ++ scenarios)
  def dec(scenarioThatCausedNode: Scenario[Params, BFn, R, RFn], yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn]) =
    new Decision(List(scenarioThatCausedNode.because.get), yes, no, scenarioThatCausedNode)
  def dec(scenariosWithBecause: List[Scenario[Params, BFn, R, RFn]], yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn]) =
    new Decision(scenariosWithBecause.map(_.because).collect { case Some(b) => b }, yes, no, scenariosWithBecause.head)
}

