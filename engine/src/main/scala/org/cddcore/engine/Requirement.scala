package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions
import org.cddcore.utilities._
import org.cddcore.engine.builder.DecisionTree
import org.cddcore.engine.builder.Conclusion
trait Reportable {
  val textOrder: Int
}
trait ReportableWithTemplate extends Reportable {
  val template: String
}
trait WithCddDisplayProcessor {
  def ldp: CddDisplayProcessor
}

trait ReportableWithoutUrl extends Reportable {

}

object Reportable {
  private final val count = new AtomicInteger(0)
  def nextTextOrder = count.getAndIncrement()
  def compare[R](expected: Either[Exception, R], actual: Either[Exception, R]) = {
    (expected, actual) match {
      case (Left(le), Left(re)) => le.getClass == re.getClass //expected.getClass.isAssignableFrom(actual.getClass())
      case (Right(lr), Right(rr)) => lr == rr
      case _ => false
    }
  }
  def compareAllowingExceptionToBeMoreSpecific[R](expected: Either[Exception, R], actual: Either[Exception, R]) = {
    (expected, actual) match {
      case (Left(le), Left(re)) => expected.getClass.isAssignableFrom(actual.getClass())
      case (Right(lr), Right(rr)) => lr == rr
      case _ => false
    }
  }
}
object PathUtils {
  /** Walks up the path until it finds the first engine*/
  def findEngine(path: List[Reportable]) = findEnginePath(path).head.asInstanceOf[EngineRequirement[_, _, _, _]]
  /** Walks up the path until it finds the first engine, return a truncated path with the engine as the head */
  def findEnginePath(path: List[Reportable]): List[Reportable] = path match {
    case (_: EngineRequirement[_, _, _, _]) :: tail => path
    case _ :: tail => findEnginePath(tail)
    case _ => throw new IllegalArgumentException
  }
  /** Walks up the path until it finds the first use case */
  def findUseCase(path: List[Reportable]) = findUseCasePath(path).head.asInstanceOf[UseCase[_, _, _, _]]
  /** Walks up the path until it finds the first use case, return a truncated path with the usecase as the head */
  def findUseCasePath(path: List[Reportable]): List[Reportable] = path match {
    case (usecase: UseCase[_, _, _, _]) :: tail => path
    case h :: tail => findUseCasePath(tail)
    case _ => throw new IllegalArgumentException
  }

}

object Requirement {
  def areRequirementFieldsEqual(r1: Requirement, r2: Requirement) = {
    r1.title == r2.title &&
      r1.description == r2.description &&
      r1.priority == r2.priority &&
      r1.references == r2.references
  }
  def areBuilderNodeFieldsEquals[Params, BFn, R, RFn](r1: BuilderNode[Params, BFn, R, RFn], r2: BuilderNode[Params, BFn, R, RFn]) = {
    areRequirementFieldsEqual(r1, r2) && r1.expected == r2.expected && r1.code == r2.code
  }
  def areBuilderNodeAndHolderFieldsEqual[Params, BFn, R, RFn](r1: BuilderNodeAndHolder[Params, BFn, R, RFn], r2: BuilderNodeAndHolder[Params, BFn, R, RFn]) = {
    Requirement.areBuilderNodeFieldsEquals(r1, r2) && r1.nodes == r2.nodes
  }

}

trait Titled {
  def title: Option[String]
  lazy val titleString = title.getOrElse("")
}

trait TitledReportable extends Reportable with Titled {
  def description: Option[String]
  def titleOrDescription(default: String) = title.getOrElse(description.getOrElse(default))

}

trait Requirement extends TitledReportable {
  def priority: Option[Int]
  def references: Set[Reference]
  def copyRequirement(title: Option[String] = title,
    description: Option[String] = description,
    priority: Option[Int] = priority,
    references: Set[Reference] = references): Requirement
}

trait TypedReportable[Params, BFn, R, RFn] extends Reportable

trait BuilderNode[Params, BFn, R, RFn] extends Requirement with TypedReportable[Params, BFn, R, RFn] {
  def expected: Option[Either[Exception, R]]
  def code: Option[CodeHolder[RFn]]
  def copyBuilderNode(
    expected: Option[Either[Exception, R]] = expected,
    code: Option[CodeHolder[RFn]] = code): BuilderNode[Params, BFn, R, RFn]
}

trait BuilderNodeHolder[Params, BFn, R, RFn] extends NestedHolder[BuilderNode[Params, BFn, R, RFn]] {
  def copyNodes(nodes: List[BuilderNode[Params, BFn, R, RFn]]): BuilderNodeHolder[Params, BFn, R, RFn]
}

trait FoldingBuilderNodeHolder[Params, BFn, R, RFn, FullR] extends BuilderNodeHolder[Params, BFn, R, RFn] {
  def foldingFn: (FullR, R) => FullR
  def initialValue: CodeHolder[() => FullR]
}

trait BuilderNodeAndHolder[Params, BFn, R, RFn] extends BuilderNode[Params, BFn, R, RFn] with BuilderNodeHolder[Params, BFn, R, RFn]

object ReportableHelper {
  implicit def toReportableHelper[Params, BFn, R, RFn](r: NestedHolder[Reportable] with TypedReportable[Params, BFn, R, RFn]) = new ReportableHelper[Params, BFn, R, RFn](r)
  implicit def toReportableHelper[Params, BFn, R, RFn](r: BuilderNodeAndHolder[Params, BFn, R, RFn] with TypedReportable[Params, BFn, R, RFn]) = new ReportableHelper[Params, BFn, R, RFn](r.asInstanceOf[NestedHolder[Reportable] with TypedReportable[Params, BFn, R, RFn]])
}

class ReportableHelper[Params, BFn, R, RFn](r: NestedHolder[Reportable] with TypedReportable[Params, BFn, R, RFn]) {
  lazy val scenarios = r.all(classOf[Scenario[Params, BFn, R, RFn]]).toList.sortBy(_.textOrder)
  lazy val useCases = r.all(classOf[UseCase[Params, BFn, R, RFn]]).toList.sortBy(_.textOrder)
  lazy val documents =
    (r :: r.toList).foldLeft(Set[Document]())((acc, r) =>
      r match {
        case r: Requirement => acc ++ r.references.flatMap(_.document);
        case _ => acc
      }).toList.sortBy((x) => x.textOrder)

}

//case class Project(
//  val title: Option[String] = None,
//  val description: Option[String] = None,
//  val priority: Option[Int] = None,
//  val nodes: List[Reportable] = List(),
//  val references: Set[Reference] = Set(),
//  val textOrder: Int = Reportable.nextTextOrder) extends Requirement with NestedHolder[Reportable] {
//  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
//    new Project(title, description, priority, nodes, references, textOrder)
//  override def toString = s"Project(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
//  override def hashCode = (title.hashCode() + description.hashCode()) / 2
//  override def equals(other: Any) = other match {
//    case p: Project => Requirement.areRequirementFieldsEqual(this, p)
//    case _ => false
//  }
//}

case class FoldingEngineDescription[Params, BFn, R, RFn, FullR](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[Params, BFn, R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val foldingFn: (FullR, R) => FullR,
  val initialValue: CodeHolder[() => FullR],
  val textOrder: Int = Reportable.nextTextOrder)
  extends EngineRequirement[Params, BFn, R, RFn] with FoldingBuilderNodeHolder[Params, BFn, R, RFn, FullR] with TypedReportable[Params, BFn, R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FoldingEngineDescription[Params, BFn, R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[Params, BFn, R, RFn] =
    new FoldingEngineDescription[Params, BFn, R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyNodes(nodes: List[BuilderNode[Params, BFn, R, RFn]]) =
    new FoldingEngineDescription[Params, BFn, R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)

  def pathsIncludingTreeAndEngine(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] = {
    val path = this :: pathNotIncludingThis
    path :: nodes.flatMap {
      case e: EngineDescription[Params, BFn, R, RFn] => e.pathsIncludingTreeAndEngine(path)
      case h: NestedHolder[Reportable] => h.pathsIncludingSelf(path)
      case r => List(r :: path)
    }
  }
  def requirementsIncludingTree(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] = ???

  override def toString = s"FoldingEngineDescription(${initialValue.description}, $foldingFn, nodes=${nodes.mkString(", ")}"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case fe: FoldingEngineDescription[Params, BFn, R, RFn, FullR] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, fe) &&
      (foldingFn == fe.foldingFn) &&
      (initialValue == fe.initialValue)
    case _ => false
  }
}
case class EngineDescription[Params, BFn, R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[Params, BFn, R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val tree: Option[DecisionTree[Params, BFn, R, RFn]] = None,
  val textOrder: Int = Reportable.nextTextOrder)
  extends EngineRequirement[Params, BFn, R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineDescription[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, tree, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[Params, BFn, R, RFn] =
    new EngineDescription[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, tree, textOrder)
  def copyNodes(nodes: List[BuilderNode[Params, BFn, R, RFn]]): BuilderNodeAndHolder[Params, BFn, R, RFn] =
    new EngineDescription[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, tree, textOrder)
  def pathsIncludingTreeAndEngine(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] = {
    pathsIncludingSelf(pathNotIncludingThis).toList ::: tree.toList.flatMap(_.treePathsWithElseClause(this :: pathNotIncludingThis).toList)
  }
  def requirementsIncludingTree(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] =
    pathsFrom(pathNotIncludingThis).toList ::: tree.toList.flatMap(_.treePathsWithElseClause(pathNotIncludingThis).toList)

  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case ed: EngineDescription[Params, BFn, R, RFn] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, ed) && tree == ed.tree
    case _ => false
  }
  override def toString = s"EngineDescription(${title.getOrElse("")}, nodes=${nodes.mkString(",")}, treeSet=${tree.isDefined})"
}

case class UseCase[Params, BFn, R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[Params, BFn, R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends BuilderNodeAndHolder[Params, BFn, R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new UseCase[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[Params, BFn, R, RFn] =
    new UseCase[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyNodes(nodes: List[BuilderNode[Params, BFn, R, RFn]]): BuilderNodeAndHolder[Params, BFn, R, RFn] =
    new UseCase[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  override def toString = s"UseCase(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case uc: UseCase[Params, BFn, R, RFn] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, uc)
    case _ => false
  }
}

case class Scenario[Params, BFn, R, RFn](
  val params: Params,
  val title: Option[String] = None,
  val description: Option[String] = None,
  val because: Option[CodeHolder[BFn]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = List(),
  val configurators: List[(Params) => Unit] = List(),
  val textOrder: Int = Reportable.nextTextOrder)(implicit val cdp: CddDisplayProcessor) extends BuilderNode[Params, BFn, R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[Params, BFn, R, RFn] =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)
  def copyScenario(because: Option[CodeHolder[BFn]] = because, assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = assertions, configurators: List[(Params) => Unit] = configurators) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)

  def prettyPrintExpected: String = expected match {
    case Some(Left(e)) => "throws " + e.getClass
    case Some(Right(v)) => cdp(v)
    case _ => "No expected"
  }
  def htmlPrintExpected: String = expected match {
    case Some(Left(e)) => "throws " + e.getClass
    case Some(Right(v)) => cdp.html(v)
    case _ => "No expected"
  }

  def actualCode(expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]) = code.getOrElse(expectedToCode(expected.getOrElse(throw NoExpectedException(this))))
  def executeConfigurators = configurators.foreach((c) => c(params))
  override def hashCode = (title.hashCode() + params.hashCode()) / 2
  override def equals(other: Any) = other match {
    case s: Scenario[Params, BFn, R, RFn] => Requirement.areBuilderNodeFieldsEquals(this, s) &&
      (s.params == params) && (s.because == because) && (s.assertions == assertions) && (s.configurators == configurators) && (s.expected == expected)
    case _ => false
  }
  override def toString = s"Scenario($params,$title,$description,$because,$code,$priority,$expected,$references,$assertions,$configurators)"
}

case class Document(
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val url: Option[String] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends Requirement {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Document(title, description, priority, url, references)
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case d: Document => Requirement.areRequirementFieldsEqual(this, d) && (url == d.url)
    case _ => false
  }

}

/** A reference is usually a link to a document. The 'ref' is a string of the form a.b.c..., where the fragments are string not containing a dot. The sort order is based on each fragment*/
case class Reference(ref: String = "", document: Option[Document] = None) extends Comparable[Reference] {
  def compareTo(other: Reference): Int = {
    val left = ref.split("\\.")
    val right = other.ref.split("\\.")
    val zipped = left.zipAll(right, "0", "0")
    zipped.map((f) => {
      val (l, r) = f
      try {
        val lInt = l.toInt
        val rInt = r.toInt
        lInt - rInt
      } catch {
        case e: Throwable => {
          l.compareTo(r)
        }
      }
    }).find((f) => f != 0).getOrElse(0)
  }

}

case class RequirementForTest(
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends Requirement {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new RequirementForTest(title, description, priority, references, textOrder)
}

case class BuilderNodeForTest[Params, BFn, R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val expected: Option[Either[Exception, R]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends BuilderNode[Params, BFn, R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new BuilderNodeForTest[Params, BFn, R, RFn](title, description, priority, references, expected, code)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[Params, BFn, R, RFn] =
    new BuilderNodeForTest[Params, BFn, R, RFn](title, description, priority, references, expected, code, textOrder)
}

case class BuilderNodeHolderForTest[Params, BFn, R, RFn](nodes: List[BuilderNode[Params, BFn, R, RFn]] = List()) extends BuilderNodeHolder[Params, BFn, R, RFn] {
  def copyNodes(nodes: List[BuilderNode[Params, BFn, R, RFn]]) = new BuilderNodeHolderForTest[Params, BFn, R, RFn](nodes)
}
case class BuilderNodeAndHolderForTest[Params, BFn, R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val expected: Option[Either[Exception, R]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val nodes: List[BuilderNode[Params, BFn, R, RFn]],
  val textOrder: Int = Reportable.nextTextOrder) extends BuilderNodeAndHolder[Params, BFn, R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new BuilderNodeAndHolderForTest[Params, BFn, R, RFn](title, description, priority, references, expected, code, nodes, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[Params, BFn, R, RFn] =
    new BuilderNodeAndHolderForTest[Params, BFn, R, RFn](title, description, priority, references, expected, code, nodes, textOrder)
  def copyNodes(nodes: List[BuilderNode[Params, BFn, R, RFn]]) =
    new BuilderNodeAndHolderForTest[Params, BFn, R, RFn](title, description, priority, references, expected, code, nodes, textOrder)

}