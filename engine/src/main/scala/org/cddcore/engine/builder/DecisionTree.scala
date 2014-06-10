package org.cddcore.engine.builder

import scala.language.implicitConversions
import org.cddcore.utilities.Lens
import org.cddcore.utilities.CodeHolder
import org.cddcore.engine._
import org.cddcore.utilities.NestedHolder
import org.cddcore.utilities.Exceptions

class DecisionTreeLens[Params, R](val creator: (DecisionTreeNode[Params, R]) => DecisionTree[Params, R] = (root: DecisionTreeNode[Params, R]) => new SimpleDecisionTree(root, false)) {
  val rootL = Lens[DecisionTree[Params, R], DecisionTreeNode[Params, R]](
    (tree) => tree.root,
    (tree, root) => creator(root),
    Some("rootL"))

  //  def exceptionMapL(asRequirement: BuilderNodeAndHolder[Params, BFn,R, RFn] ) =
  //    Lens[DecisionTree[Params, R],ExceptionMap](
  //      (tree) => tree.buildExceptions,
  //      (tree, buildExceptions) => creator(asRequirement)(tree.root, buildExceptions),
  //      Some("exceptionMapL"))

  def toDecisionL = Lens[DecisionTreeNode[Params, R], Decision[Params, R]](
    (d) => d.asInstanceOf[Decision[Params, R]],
    (d, x) => x,
    Some("toDecisionL"))
  def toConclusionL = Lens[DecisionTreeNode[Params, R], Conclusion[Params, R]](
    (d) => d.asInstanceOf[Conclusion[Params, R]],
    (d, x) => x,
    Some("toConclusionL"))
  def yesL = Lens[Decision[Params, R], DecisionTreeNode[Params, R]](
    (d) => d.yes,
    (d, x) => d.copy(yes = x),
    Some("yesL"))
  def noL = Lens[Decision[Params, R], DecisionTreeNode[Params, R]](
    (d) => d.no,
    (d, x) => d.copy(no = x),
    Some("noL"))
}
object ElseClause {
  private val instance = new ElseClause
  def apply() = instance
}

class ElseClause(val textOrder: Int = Reportable.nextTextOrder) extends ReportableWithoutUrl

trait DecisionTree[Params, R] extends NestedHolder[DecisionTreeNode[Params, R]] with Reportable {
  def root: DecisionTreeNode[Params, R]
  def rootIsDefault: Boolean
  def nodes = List(root)
  def treePathsWithElseClause[Params, R](pathNotIncludingTree: List[Reportable]): List[List[Reportable]] =
    (this :: pathNotIncludingTree) :: treePaths(this :: pathNotIncludingTree, root)

  //builds up in reverse order
  private def treePaths[Params, R](path: List[Reportable], node: DecisionTreeNode[Params, R]): List[List[Reportable]] = {
    node match {
      case c: Conclusion[Params, R] => List((c :: path))
      case d: Decision[Params, R] => {
        val dPath = (d :: path)
        val yesPaths = treePaths(dPath, d.yes)
        val noPaths = treePaths(dPath, d.no)
        dPath :: yesPaths ::: List(ElseClause() :: dPath) ::: noPaths
      }
    }
  }
}

case class SimpleDecisionTree[Params, R](root: DecisionTreeNode[Params, R], val rootIsDefault: Boolean = true, textOrder: Int = Reportable.nextTextOrder) extends DecisionTree[Params, R] {
  override def hashCode = root.hashCode()
  override def equals(other: Any) = other match {
    case sdt: SimpleDecisionTree[Params, R] => root == sdt.root && rootIsDefault == sdt.rootIsDefault
    case _ => false
  }
}
trait AnyDecisionTreeNode extends Reportable {
  def toDecisionTreeNode[Params, R] = this.asInstanceOf[DecisionTreeNode[Params, R]]
  def containsConclusion(c: AnyConclusion): Boolean
  val conclusions: List[AnyConclusion]
}

sealed trait DecisionTreeNode[Params, R] extends Reportable with AnyDecisionTreeNode {
  def scenarios: List[Scenario[Params, R]]
  def asConclusion = asInstanceOf[Conclusion[Params, R]]
  def asDecision = asInstanceOf[Decision[Params, R]]
}

trait AnyConclusion extends AnyDecisionTreeNode {
  def toConclusion[Params, R] = this.asInstanceOf[Conclusion[Params, R]]
  def toCode[Params, R] = toConclusion[Params, R].code
  lazy val conclusions = List(this)
  def toScenarios: List[AnyScenario] = toConclusion.scenarios

}

case class Conclusion[Params, R](code: CodeHolder[(Params) => R], scenarios: List[Scenario[Params, R]], textOrder: Int = Reportable.nextTextOrder) extends DecisionTreeNode[Params, R] with AnyConclusion {
  def containsConclusion(c: AnyConclusion) = eq(c)
  override def hashCode = textOrder
  override def equals(other: Any) = other match {
    case c: Conclusion[Params, R] => c.code == code && c.scenarios == scenarios
    case _ => false
  }
}

trait AnyDecision extends AnyDecisionTreeNode {
  def toDecision[Params, R] = this.asInstanceOf[Decision[Params, R]]
  def toYes[Params, R] = toDecision[Params, R].yes
  def toNo[Params, R] = toDecision[Params, R].no
  def toPrettyString[Params, R] = toDecision[Params, R].prettyString
  lazy val conclusions = toYes.conclusions ::: toNo.conclusions
}

case class Decision[Params, R](because: List[CodeHolder[(Params) => Boolean]], yes: DecisionTreeNode[Params, R], no: DecisionTreeNode[Params, R], scenarioThatCausedNode: Scenario[Params, R], textOrder: Int = Reportable.nextTextOrder)
  extends DecisionTreeNode[Params, R] with NestedHolder[DecisionTreeNode[Params, R]] with AnyDecision {
  def containsConclusion(c: AnyConclusion) = yes.containsConclusion(c) || no.containsConclusion(c)
  def scenarios = yes.scenarios ++ no.scenarios
  def isTrue(bc: ((Params) => Boolean) => Boolean) = because.foldLeft(false)((acc, ch) => acc || bc(ch.fn))
  def prettyString = because.map(_.pretty).mkString(" or ")
  def nodes = List(yes, no)
  override lazy val hashCode = (because.hashCode + yes.hashCode() + no.hashCode()) / 3
  override def equals(other: Any) = other match {
    case d: Decision[Params, R] => d.because == because && d.yes == yes && d.no == no
    case _ => false
  }
}

class MakeClosures[Params, R] {
  type S = Scenario[Params, R]
  type Result = Either[Exception, R]
  type BFn = (Params) => Boolean
  type RFn = (Params) => R
  type BecauseClosure = (BFn) => Boolean
  type ResultClosure = (RFn) => R

  def makeBecauseClosure(params: Params): BecauseClosure = ((bfn) => wrapBecause(params, bfn(params)))
  def makeBecauseClosure(s: Scenario[Params, R]): BecauseClosure = ((bfn) => wrapBecause(s, { s.executeConfigurators; bfn(s.params) }))
  def makeResultClosure(params: Params): ResultClosure = (rfn) =>rfn(params)
  def makeResultClosure(scenarioWithParams: Scenario[Params, R]): ResultClosure = ((rfn) => { scenarioWithParams.executeConfigurators; rfn(scenarioWithParams.params) })

  protected def wrapBecause[X](s: S, x: => X): X = try { x } catch { case e: Exception => throw BecauseClauseScenarioException(s, e) }
  protected def wrapBecause[X](p: Params, x: => X): X = try { x } catch { case e: Exception => throw BecauseClauseException(p, e) }

  def evaluateBecause(scenarioWithbecause: Scenario[Params, R], scenarioWithParams: Scenario[Params, R]): Boolean =
    makeBecauseClosure(scenarioWithParams)(scenarioWithbecause.      because.getOrElse(throw new IllegalArgumentException(s"Scenario doesn't have because\scenarioWithbecause")).fn)
  def evaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, R]) = makeResultClosure(scenarioWithParams)(rfn)
  def safeEvaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, R]) = Exceptions(makeResultClosure(scenarioWithParams)(rfn))
}

case class SimpleEvaluateTree[Params, R](
  val makeClosures: MakeClosures[Params, R],
  val lens: DecisionTreeLens[Params, R],
  val validator: ValidateScenario[Params, R]) extends EvaluateTree[Params, R] {
}

trait EvaluateTree[Params, R] {
  type DT = DecisionTree[Params, R]
  type DTN = DecisionTreeNode[Params, R]

  val makeClosures: MakeClosures[Params, R]
  val validator: ValidateScenario[Params, R]
  val lens: DecisionTreeLens[Params, R]
  import makeClosures._
  import lens._

  def findLensToConclusion(root: DTN, s: S): Lens[DT, DTN] = findLensToConclusion(root, makeBecauseClosure(s), rootL)
  def findLensToConclusion(root: DTN, bc: (BFn) => Boolean): Lens[DT, DTN] = findLensToConclusion(root, bc, rootL)
  def findLensToLastDecisionNode(root: DTN, s: S): Option[Lens[DT, Decision[Params, R]]] = findLensToLastDecisionNode(root, makeBecauseClosure(s), rootL.andThen(toDecisionL))

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
      case d: Decision[Params, R] => d.isTrue(becauseClosure) match {
        case true => findPathToConclusionPrim(d.yes, becauseClosure, d :: path)
        case false => findPathToConclusionPrim(d.no, becauseClosure, d :: path)
      }
      case c: Conclusion[Params, R] => c :: path
    }
  }
  def safeEvaluate(tree: DT, scenarioWithParams: S) = Exceptions(evaluate(tree, scenarioWithParams))

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

  def findConclusion(tree: DT, bc: BecauseClosure): Conclusion[Params, R] = findConclusion(tree.root, bc)

  def findConclusion(root: DTN, bc: BecauseClosure): Conclusion[Params, R] = {
    root match {
      case c: Conclusion[Params, R] => c
      case d: Decision[Params, R] => d.isTrue(bc) match {
        case true => findConclusion(d.yes, bc)
        case false => findConclusion(d.no, bc)
      }
    }
  }

  type LensToNode = Lens[DecisionTree[Params, R], DecisionTreeNode[Params, R]]

  protected def findLensToConclusion(root: DTN, bc: BecauseClosure, soFar: LensToNode): Lens[DT, DTN] =
    root match {
      case c: Conclusion[Params, R] => soFar
      case d: Decision[Params, R] => d.isTrue(bc) match {
        case true => findLensToConclusion(d.yes, bc, soFar.andThen(toDecisionL).andThen(yesL))
        case false => findLensToConclusion(d.no, bc, soFar.andThen(toDecisionL).andThen(noL))
      }
    }
  protected def findLensToLastDecisionNode(root: DTN, bc: BecauseClosure, soFar: Lens[DT, Decision[Params, R]]): Option[Lens[DT, Decision[Params, R]]] =
    root match {
      case d: Decision[Params, R] => {
        def returnMeOrRecurse(child: DTN, nextSoFar: LensToNode): Option[Lens[DT, Decision[Params, R]]] =
          child match {
            case c: Conclusion[Params, R] => Some(soFar)
            case d: Decision[Params, R] => findLensToLastDecisionNode(child, bc, nextSoFar.andThen(toDecisionL))
          }
        d.isTrue(bc) match {
          case true => returnMeOrRecurse(d.yes, soFar.andThen(yesL))
          case false => returnMeOrRecurse(d.no, soFar.andThen(noL))
        }
      }
      case c: Conclusion[Params, R] => None
    }
}

trait DecisionTreeBuilderForTests[Params, R] {
  def expectedToCode: (Either[Exception, R]) => CodeHolder[(Params) => R]
  def scen(params: Params, title: String = null, description: Option[String] = None, because: Option[CodeHolder[(Params) => Boolean]] = None,
    code: Option[CodeHolder[(Params) => R]] = None, priority: Option[Int] = None, expected: Option[Either[Exception, R]] = None,
    references: Set[Reference] = Set()) =
    new Scenario[Params, R](params, Option(title), description, because, code, priority, expected, references)
  def uc(title: String = null) = new UseCase[Params, R](Option(title))
  def conc(scenario: Scenario[Params, R], scenarios: Scenario[Params, R]*) =
    new Conclusion[Params, R](scenario.actualCode(expectedToCode), List(scenario) ++ scenarios)
  def dec(scenarioThatCausedNode: Scenario[Params, R], yes: DecisionTreeNode[Params, R], no: DecisionTreeNode[Params, R]) =
    new Decision(List(scenarioThatCausedNode.because.get), yes, no, scenarioThatCausedNode)
  def dec(scenariosWithBecause: List[Scenario[Params, R]], yes: DecisionTreeNode[Params, R], no: DecisionTreeNode[Params, R]) =
    new Decision(scenariosWithBecause.map(_.because).collect { case Some(b) => b }, yes, no, scenariosWithBecause.head)
}

