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
      case (Left(le), Left(re))   => le.getClass == re.getClass //expected.getClass.isAssignableFrom(actual.getClass())
      case (Right(lr), Right(rr)) => lr == rr
      case _                      => false
    }
  }
  def compareAllowingExceptionToBeMoreSpecific[R](expected: Either[Exception, R], actual: Either[Exception, R]) = {
    (expected, actual) match {
      case (Left(le), Left(re))   => expected.getClass.isAssignableFrom(actual.getClass())
      case (Right(lr), Right(rr)) => lr == rr
      case _                      => false
    }
  }
  def titleOf(r: Reportable) = r match { case r: TitledReportable => r.title; case _ => None }
}
object PathUtils {
  /** Walks up the path until it finds the first engine*/
  def findEngine(path: List[Reportable]) = findEnginePath(path).head.asInstanceOf[EngineRequirement[_, _]]
  /** Walks up the path until it finds the first engine, return a truncated path with the engine as the head */
  def findEnginePath(path: List[Reportable]): List[Reportable] = path match {
    case (_: EngineRequirement[_, _]) :: tail => path
    case _ :: tail                            => findEnginePath(tail)
    case _                                    => throw new IllegalArgumentException
  }
  /** Walks up the path until it finds the first use case */
  def findUseCase(path: List[Reportable]) = findUseCasePath(path).head.asInstanceOf[UseCase[_, _]]
  /** Walks up the path until it finds the first use case, return a truncated path with the usecase as the head */
  def findUseCasePath(path: List[Reportable]): List[Reportable] = path match {
    case (usecase: UseCase[_, _]) :: tail => path
    case h :: tail                        => findUseCasePath(tail)
    case _                                => throw new IllegalArgumentException
  }

}

object Requirement {
  def areRequirementFieldsEqual(r1: Requirement, r2: Requirement) = {
    r1.title == r2.title &&
      r1.description == r2.description &&
      r1.priority == r2.priority &&
      r1.references == r2.references
  }
  def areBuilderNodeFieldsEquals[Params, R](r1: BuilderNode[Params, R], r2: BuilderNode[Params, R]) = {
    areRequirementFieldsEqual(r1, r2) && r1.expected == r2.expected && r1.code == r2.code
  }
  def areBuilderNodeAndHolderFieldsEqual[Params, R](r1: BuilderNodeAndHolder[Params, R], r2: BuilderNodeAndHolder[Params, R]) = {
    Requirement.areBuilderNodeFieldsEquals(r1, r2) && r1.nodes == r2.nodes
  }

}

trait Titled {
  def title: Option[String]
  lazy val titleString = title.getOrElse("")
}

trait TitledReportable extends Reportable with Titled {
  def description: Option[String]
  def titleOrDescription(default: => String) = title.getOrElse(description.getOrElse(default))

}

trait Requirement extends TitledReportable {
  def priority: Option[Int]
  def references: Set[Reference]
  def copyRequirement(title: Option[String] = title,
                      description: Option[String] = description,
                      priority: Option[Int] = priority,
                      references: Set[Reference] = references): Requirement
}

trait TypedReportable[Params, R] extends Reportable

trait BuilderNode[Params, R] extends Requirement with TypedReportable[Params, R] {
  def expected: Option[Either[Exception, R]]
  def code: Option[CodeHolder[(Params) => R]]
  def copyBuilderNode(
    expected: Option[Either[Exception, R]] = expected,
    code: Option[CodeHolder[(Params) => R]] = code): BuilderNode[Params, R]
}

trait BuilderNodeHolder[Params, R] extends NestedHolder[BuilderNode[Params, R]] {
  def copyNodes(nodes: List[BuilderNode[Params, R]]): BuilderNodeHolder[Params, R]
}

trait FoldingBuilderNodeHolder[Params, R, FullR] extends BuilderNodeHolder[Params, R] {
  def foldingFn: (FullR, R) => FullR
  def initialValue: CodeHolder[() => FullR]
}

trait BuilderNodeAndHolder[Params, R] extends BuilderNode[Params, R] with BuilderNodeHolder[Params, R]

object ReportableHelper {
  implicit def toReportableHelper[Params, R](r: NestedHolder[Reportable] with TypedReportable[Params, R]) = new ReportableHelper[Params, R](r)
  implicit def toReportableHelper[Params, R](r: BuilderNodeAndHolder[Params, R] with TypedReportable[Params, R]) = new ReportableHelper[Params, R](r.asInstanceOf[NestedHolder[Reportable] with TypedReportable[Params, R]])
}

class ReportableHelper[Params, R](r: NestedHolder[Reportable] with TypedReportable[Params, R]) {
  lazy val scenarios = r.all(classOf[Scenario[Params, R]]).toList.sortBy(_.textOrder)
  lazy val useCases = r.all(classOf[UseCase[Params, R]]).toList.sortBy(_.textOrder)
  lazy val documents =
    (r :: r.toList).foldLeft(Set[Document]())((acc, r) =>
      r match {
        case r: Requirement => acc ++ r.references.flatMap(_.document);
        case _              => acc
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

case class FoldingEngineDescription[Params, R, FullR](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[(Params) => R]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[Params, R]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val foldingFn: (FullR, R) => FullR,
  val initialValue: CodeHolder[() => FullR],
  val textOrder: Int = Reportable.nextTextOrder)
    extends EngineRequirement[Params, R] with FoldingBuilderNodeHolder[Params, R, FullR] with TypedReportable[Params, R] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FoldingEngineDescription[Params, R, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[(Params) => R]] = code): BuilderNode[Params, R] =
    new FoldingEngineDescription[Params, R, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyNodes(nodes: List[BuilderNode[Params, R]]) =
    new FoldingEngineDescription[Params, R, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)

  def pathsIncludingTreeAndEngine(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] = {
    val path = this :: pathNotIncludingThis
    path :: nodes.flatMap {
      case e: EngineDescription[Params, R] => e.pathsIncludingTreeAndEngine(path)
      case h: NestedHolder[Reportable]     => h.pathsIncludingSelf(path)
      case r                               => List(r :: path)
    }
  }
  def requirementsIncludingTree(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] = ???

  override def toString = s"FoldingEngineDescription(${initialValue.description}, $foldingFn, nodes=${nodes.mkString(", ")}"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case fe: FoldingEngineDescription[Params, R, FullR] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, fe) &&
      (foldingFn == fe.foldingFn) &&
      (initialValue == fe.initialValue)
    case _ => false
  }
}
case class EngineDescription[Params, R](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[(Params) => R]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[Params, R]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val tree: Option[DecisionTree[Params, R]] = None,
  val textOrder: Int = Reportable.nextTextOrder)
    extends EngineRequirement[Params, R] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineDescription[Params, R](title, description, code, priority, nodes, expected, references, tree, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[(Params) => R]] = code): BuilderNode[Params, R] =
    new EngineDescription[Params, R](title, description, code, priority, nodes, expected, references, tree, textOrder)
  def copyNodes(nodes: List[BuilderNode[Params, R]]): BuilderNodeAndHolder[Params, R] =
    new EngineDescription[Params, R](title, description, code, priority, nodes, expected, references, tree, textOrder)
  def pathsIncludingTreeAndEngine(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] = {
    pathsIncludingSelf(pathNotIncludingThis).toList ::: tree.toList.flatMap(_.treePathsWithElseClause(this :: pathNotIncludingThis).toList)
  }
  def requirementsIncludingTree(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] =
    pathsFrom(pathNotIncludingThis).toList ::: tree.toList.flatMap(_.treePathsWithElseClause(pathNotIncludingThis).toList)

  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case ed: EngineDescription[Params, R] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, ed) && tree == ed.tree
    case _                                => false
  }
  override def toString = s"EngineDescription(${title.getOrElse("")}, nodes=${nodes.mkString(",")}, treeSet=${tree.isDefined})"
}

case class UseCase[Params, R](
    val title: Option[String] = None,
    val description: Option[String] = None,
    val code: Option[CodeHolder[(Params) => R]] = None,
    val priority: Option[Int] = None,
    val nodes: List[BuilderNode[Params, R]] = List(),
    val expected: Option[Either[Exception, R]] = None,
    val references: Set[Reference] = Set(),
    val textOrder: Int = Reportable.nextTextOrder) extends BuilderNodeAndHolder[Params, R] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new UseCase[Params, R](title, description, code, priority, nodes, expected, references, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[(Params) => R]] = code): BuilderNode[Params, R] =
    new UseCase[Params, R](title, description, code, priority, nodes, expected, references, textOrder)
  def copyNodes(nodes: List[BuilderNode[Params, R]]): BuilderNodeAndHolder[Params, R] =
    new UseCase[Params, R](title, description, code, priority, nodes, expected, references, textOrder)
  override def toString = s"UseCase(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case uc: UseCase[Params, R] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, uc)
    case _                      => false
  }
}

trait AnyScenario extends Reportable with Titled {
  def toScenario[Params, R] = this.asInstanceOf[Scenario[Params, R]]
  def toBecause[Params, R] = toScenario[Params, R].because
  def toParams[Params, R] = toScenario[Params, R].params
  def toExpected[Params, R] = toScenario[Params, R].expected
  def toCode[Params, R] = toScenario[Params, R].code
  def paramsString: String
}

case class Scenario[Params, R](
    val params: Params,
    val title: Option[String] = None,
    val description: Option[String] = None,
    val because: Option[CodeHolder[(Params) => Boolean]] = None,
    val code: Option[CodeHolder[(Params) => R]] = None,
    val priority: Option[Int] = None,
    val expected: Option[Either[Exception, R]] = None,
    val references: Set[Reference] = Set(),
    val assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = List(),
    val configurators: List[(Params) => Unit] = List(),
    val textOrder: Int = Reportable.nextTextOrder)(implicit val cdp: CddDisplayProcessor) extends AnyScenario with BuilderNode[Params, R] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Scenario[Params, R](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[(Params) => R]] = code): BuilderNode[Params, R] =
    new Scenario[Params, R](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)
  def copyScenario(because: Option[CodeHolder[(Params) => Boolean]] = because, assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = assertions, configurators: List[(Params) => Unit] = configurators) =
    new Scenario[Params, R](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)

  def paramsString = cdp(params)
  def htmlPrintExpected: String = expected match {
    case Some(Left(e))  => "throws " + e.getClass
    case Some(Right(v)) => cdp.html(v)
    case _              => "No expected"
  }

  def actualCode(expectedToCode: (Either[Exception, R]) => CodeHolder[(Params) => R]) = code.getOrElse(expectedToCode(expected.getOrElse(throw NoExpectedException(this))))
  def executeConfigurators = configurators.foreach((c) => c(params))
  override def hashCode = (title.hashCode() + params.hashCode()) / 2
  override def equals(other: Any) = other match {
    case s: Scenario[Params, R] => Requirement.areBuilderNodeFieldsEquals(this, s) &&
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
    case _           => false
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

case class BuilderNodeForTest[Params, R](
    val title: Option[String] = None,
    val description: Option[String] = None,
    val priority: Option[Int] = None,
    val references: Set[Reference] = Set(),
    val expected: Option[Either[Exception, R]] = None,
    val code: Option[CodeHolder[(Params) => R]] = None,
    val textOrder: Int = Reportable.nextTextOrder) extends BuilderNode[Params, R] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new BuilderNodeForTest[Params, R](title, description, priority, references, expected, code)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[(Params) => R]] = code): BuilderNode[Params, R] =
    new BuilderNodeForTest[Params, R](title, description, priority, references, expected, code, textOrder)
}

case class BuilderNodeHolderForTest[Params, R](nodes: List[BuilderNode[Params, R]] = List()) extends BuilderNodeHolder[Params, R] {
  def copyNodes(nodes: List[BuilderNode[Params, R]]) = new BuilderNodeHolderForTest[Params, R](nodes)
}
case class BuilderNodeAndHolderForTest[Params, R](
    val title: Option[String] = None,
    val description: Option[String] = None,
    val priority: Option[Int] = None,
    val references: Set[Reference] = Set(),
    val expected: Option[Either[Exception, R]] = None,
    val code: Option[CodeHolder[(Params) => R]] = None,
    val nodes: List[BuilderNode[Params, R]],
    val textOrder: Int = Reportable.nextTextOrder) extends BuilderNodeAndHolder[Params, R] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new BuilderNodeAndHolderForTest[Params, R](title, description, priority, references, expected, code, nodes, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[(Params) => R]] = code): BuilderNode[Params, R] =
    new BuilderNodeAndHolderForTest[Params, R](title, description, priority, references, expected, code, nodes, textOrder)
  def copyNodes(nodes: List[BuilderNode[Params, R]]) =
    new BuilderNodeAndHolderForTest[Params, R](title, description, priority, references, expected, code, nodes, textOrder)

}