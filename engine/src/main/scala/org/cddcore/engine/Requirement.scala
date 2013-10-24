package org.cddcore.engine



trait Requirement {
  def title: Option[String]
  def titleString = title.getOrElse("")
  def titleOrDescription(default: String): String = title.getOrElse(description.getOrElse(default))

  def description: Option[String]
  def priority: Int
  def references: List[Reference]
  def templateName: String = getClass.getSimpleName()
}

trait Test extends Requirement {
  def optCode: Option[CodeHolder]
  def expected: Option[ROrException[_]]
  def because: Option[CodeHolder]
  
  def params: List[Any]
  def paramPrinter: LoggerDisplayProcessor
}



object RequirementsVisitor {
  implicit def tupleToVisitor(t: Tuple3[(RequirementHolder) => _, (Requirement) => _, (RequirementHolder) => _]) =
    RequirementsVisitor(t._1, t._2, t._3)
}
case class RequirementsVisitor(holderFnStart: (RequirementHolder) => _, childFn: (Requirement) => _, holderFnEnd: (RequirementHolder) => _)

trait RequirementsFolder[Acc] {
  def holderFnStart: (Acc, RequirementHolder) => Acc
  def childFn: (Acc, Requirement) => Acc
  def holderFnEnd: (Acc, RequirementHolder) => Acc
}
object RequirementsFolder {
  implicit def tupleToFolder[Acc](t: Tuple3[(Acc, RequirementHolder) => Acc, (Acc, Requirement) => Acc, (Acc, RequirementHolder) => Acc]) = SimpleRequirementsFolder(t._1, t._2, t._3)
}
case class SimpleRequirementsFolder[Acc](holderFnStart: (Acc, RequirementHolder) => Acc, childFn: (Acc, Requirement) => Acc, holderFnEnd: (Acc, RequirementHolder) => Acc) extends RequirementsFolder[Acc]

trait RequirementHolder extends Requirement with Traversable[Requirement] {
  def children: List[Requirement]
  def foreach[U](f: Requirement => U): Unit = {
    for (c <- children) c match {
      case holder: RequirementHolder =>
        f(c); holder.foreach(f)
      case c => f(c)
    }
  }
  def walk(visitor: RequirementsVisitor) {
    visitor.holderFnStart(this)
    for (c <- children)
      c match {
        case holder: RequirementHolder => holder.walk(visitor);
        case _ => visitor.childFn(c)
      }
    visitor.holderFnEnd(this)
  }
  def fold[Acc](initial: Acc)(folder: RequirementsFolder[Acc]): Acc = {
    //I could do this functionally, but I'm not sure it would be any cleaner
    var acc: Acc = folder.holderFnStart(initial, this)
    for (c <- children)
      c match {
        case holder: RequirementHolder =>
          acc = holder.fold(acc)(folder);
        case _ => acc = folder.childFn(acc, c)
      }
    val result: Acc = folder.holderFnEnd(acc, this)
    result
  }
}

