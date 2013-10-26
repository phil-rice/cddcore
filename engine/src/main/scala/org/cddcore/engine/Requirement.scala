package org.cddcore.engine

trait Reportable {
  
}

trait Requirement extends Reportable{
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
  def walkWithPath(visitor: (List[Requirement]) => Unit): Unit = walkWithPath(List(), visitor)

  def walkWithPath(path: List[Requirement], visitor: (List[Requirement]) => Unit): Unit = {
    val newPath = path :+ this
    visitor(newPath)
    for (c <- children)
      c match {
        case holder: RequirementHolder => holder.walkWithPath(newPath, visitor)
        case _ => visitor (newPath :+ c)
      }
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
  def foldWithPath[Acc](initial: Acc)(folder: RequirementsFolderWithPath[Acc]): Acc = foldWithPath((List(), initial))(folder)._2
  def foldWithPath[Acc](initial: (List[Requirement], Acc))(folder: RequirementsFolderWithPath[Acc]): (List[Requirement], Acc) = {
    //I could do this functionally, but I'm not sure it would be any cleaner
    val initialPath = initial._1
    val pathWithThis = initialPath :+ RequirementHolder.this
    var acc: (List[Requirement], Acc) = folder.holderFnStart(initial, this)
    for (c <- children)
      c match {
        case holder: RequirementHolder =>
          acc = holder.foldWithPath((pathWithThis, acc._2))(folder);
        case _ => acc = folder.childFn((pathWithThis, acc._2), c)
      }
    val result: (List[Requirement], Acc) = folder.holderFnEnd((initialPath, acc._2), this)
    (initialPath, result._2)
  }
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

trait RequirementsFolderWithPath[Acc] {
  def holderFnStart: ((List[Requirement], Acc), RequirementHolder) => (List[Requirement], Acc)
  def childFn: ((List[Requirement], Acc), Requirement) => (List[Requirement], Acc)
  def holderFnEnd: ((List[Requirement], Acc), RequirementHolder) => (List[Requirement], Acc)
}

trait SimpleFolderWithPath[Acc] extends RequirementsFolderWithPath[Acc]{
  def fn(reqList: List[Requirement], acc: Acc, r: Requirement):  (List[Requirement], Acc)
  def holderFnStart = (reqAndAcc, holder) => fn(reqAndAcc._1, reqAndAcc._2, holder)
  def holderFnEnd = (reqAndAcc, holder) => fn(reqAndAcc._1, reqAndAcc._2, holder)
  def childFn = (reqAndAcc, r) => fn(reqAndAcc._1, reqAndAcc._2, r)
}
