package org.cddcore.engine

import scala.concurrent.stm._
import java.lang.reflect.Field

case class Fold[T, A](initial: A, fn: (A, T) => A)

trait FragStrategy[S, Result] {
  def findResult(raw: S, paths: List[Path]): Result
  def findA[T, A](result: Result, convertor: (Result) => Option[T], fold: Fold[T, A]): Option[A]
}

case class Fragment[S, Result, T, A](fragStrategy: FragStrategy[S, Result], raw: S, paths: List[Path] = List(), convertor: Option[(Result) => Option[T]], fold: Option[Fold[T, A]] = None) {
  def \(s: String) = Fragment(fragStrategy, raw, Path(true, s) :: paths, convertor, fold)
  def \\(s: String) = Fragment(fragStrategy, raw, Path(false, s) :: paths, convertor, fold)
  def \[T](c: (Result) => Option[T]) = Fragment[S, Result, T, T](fragStrategy, raw, paths, Some(c), None)
  def \[A](fold: Fold[T, A]) = Fragment[S, Result, T, A](fragStrategy, raw, paths, convertor, Some(fold))

  lazy val result = fragStrategy.findResult(raw, paths)
  private lazy val value: Option[A] = convertor match {
    case Some(c) =>
      fold match {
        case Some(f) => fragStrategy.findA(result, c, f)
        case _ =>
          val r = c(result)
          r.asInstanceOf[Option[A]] // Can only be here without Fold, in which case T = A
      }
    case _ => None
  }
  def get() = value
  def apply(): A = value match {
    case Some(v) => v
    case _ => throw new NoSuchElementException(raw"Fragment $paths \ $convertor \ $fold \n${raw.getClass.getSimpleName}\n$raw")
  }
}
object PathMap {
  def subLists[X](l: List[X]) =
    for (i <- 1 to l.size)
      yield l.take(i)
  def allPaths(fragments: List[Fragment[_, _, _, _]]) =
    fragments.flatMap((f) => subLists(f.paths.reverse))
  def allButLast(l: List[Path]) = l.take(l.size - 1)
  def apply[Structure, Result](fragments: List[Fragment[Structure, Result, _, _]]): PathMap[Structure, Result] = {
    val elemToFragment = fragments.groupBy(_.raw)
    val map = elemToFragment.mapValues((f) => {
      val paths = allPaths(f)
      val map = paths.foldLeft(MapOfList[List[Path], List[Path]]())((acc, p) =>
        p.size match {
          case 1 => acc
          case _ => acc.put(allButLast(p), p)
        })
      map
    })
    val roots = elemToFragment.mapValues(_.map(_.paths.reverse).map(_.head).distinct.toList)
    new PathMap[Structure, Result](map, roots, elemToFragment)
  }
}

/** So this is all the ends in a parent / child map. with a list of the roots. */
case class PathMap[S, Result](val map: Map[S, MapOfList[List[Path], List[Path]]], val roots: Map[S, List[Path]], val fullPaths: Map[S, List[Fragment[S, Result, _, _]]]) {
  //  def walkPaths = roots.flatMap((p) => walkPathsSelfAndChildren(List(p)))
  //  private def walkPathsSelfAndChildren(parent: List[Path]): List[List[Path]] = parent :: map(parent).flatMap(walkPathsSelfAndChildren(_))
  def apply(structure: S, path: List[Path]) = map(structure)(path)
}

case class Path(linked: Boolean, element: String) {
  override def toString() = (if (linked) """\""" else """\\""") + element
}

case class MapOfList[K, V](val map: Map[K, List[V]] = Map[K, List[V]]()) {
  val emptyList = List[V]()
  def apply(k: K) = map.getOrElse(k, emptyList)
  def put(k: K, v: V) =
    MapOfList(map + (k -> (map.get(k) match {
      case Some(oldList) => v :: oldList
      case None => List(v)
    })))
}
case class IndentAndString(indent: String, string: String) {
  def indentedBlank = IndentAndString(indent + "  ", "")
}

class FieldSet[T](instance: Any, clazz: Class[T]) {

  private def instantiateLazyVals(clazz: Class[_]): Unit = {
    val methods = instance.getClass.getDeclaredMethods().filter((m) => m.getParameterTypes().length == 0 && clazz.isAssignableFrom(m.getReturnType()))
    for (m <- methods) {
      m.setAccessible(true)
      m.invoke(instance)
    }
  }

  private def value(f: Field) = {
    f.setAccessible(true);
    val value = f.get(instance).asInstanceOf[T];
    value
  }
  private lazy val instantiateLazyVals: Unit = instantiateLazyVals(clazz)

  lazy val fields = instance.getClass.getDeclaredFields.filter((f) => clazz.isAssignableFrom(f.getType())).toList
  lazy val fieldMap = { instantiateLazyVals; Map[Field, T]() ++ fields.map((f) => (f -> value(f))) }
  lazy val values = fields.map(fieldMap)
  def findFieldWithValue(x: Any) = fields.find((f) => fieldMap(f) == x)

}

trait Structure[S, Result] {

  protected def findFragmentsToString(fragmentFieldMap: Map[Field, Fragment[S, Result, _, _]], endToString: (Result) => String) = fragmentFieldMap.keys.map(((f) => {
    val frag = fragmentFieldMap(f)
    val resultString = try {
      frag.convertor match {
        case Some(_) => frag.apply
        case _ => "No Convertor"
      }
    } catch { case e: Throwable => e.getClass.getSimpleName() + " " + e.getMessage }
    raw"${f.getName} = ${resultString}"
  })).mkString("\n  ")

  protected def selfAndChildren(s: S, pathMap: PathMap[S, Result]): (IndentAndString, List[Path]) => IndentAndString = (acc, p) => {
    val fragments = pathMap.fullPaths(s).filter(_.paths.reverse == p)
    val valueString = fragments.size match {
      case 0 => "";
      case _ => " = " + fragments.map((f) => try {
        f.convertor match {
          case Some(c) => f.apply
          case _ => "No Convertor"
        }
      } catch { case e: Throwable => e.getClass.getSimpleName() + " " + e.getMessage }).mkString(",")
    }
    val myString = acc.indent + p.mkString("") + valueString + "\n"
    val childrensString = pathMap(s, p).foldLeft(acc.indentedBlank)(selfAndChildren(s, pathMap))
    new IndentAndString(acc.indent, acc.string + myString + childrensString.string)
  }

  protected def structuresToString(pathMap: PathMap[S, Result], structureTitle: (S) => String) = pathMap.roots.keys.map((s) => structureTitle(s) + "\n" + {
    pathMap.roots(s).foldLeft(IndentAndString("    ", ""))((acc, r) => selfAndChildren(s, pathMap)(acc, List(r)))
  }.string).mkString("\n")
}
