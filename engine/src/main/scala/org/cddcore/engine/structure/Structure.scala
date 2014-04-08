package org.cddcore.engine.structure

import scala.concurrent.stm._
import java.lang.reflect.Field
import java.lang.annotation.Annotation
import java.lang.reflect.Method
import org.cddcore.engine.Display
import org.cddcore.engine.HtmlDisplay
import org.cddcore.engine.utilities._

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
  def apply(k: K) = map.getOrElse(k, Nil)
  def put(k: K, v: V) =
    MapOfList(map + (k -> (map.get(k) match {
      case Some(oldList) => v :: oldList
      case None => List(v)
    })))
}
case class IndentAndString(indent: String, string: String) {
  def indentedBlank = IndentAndString(indent + "  ", "")
}

trait Fields[T] {
  def instance: Any
  def filter: (Field) => Boolean
  protected def instantiateLazyVals: Unit
  lazy val fields = instance.getClass.getDeclaredFields.filter(filter).toList

  lazy val fieldMap = { instantiateLazyVals; Map[Field, T]() ++ fields.map((f) => (f -> value[T](f))) }
  protected def instantiateLazyVals(filter: (Method) => Boolean): Unit = {
    val methods = instance.getClass.getDeclaredMethods().filter(filter)
    for (m <- methods) {
      m.setAccessible(true)
      m.invoke(instance)
    }
  }

  protected def instantiateLazyVals(clazz: Class[_]): Unit = instantiateLazyVals((m: Method) => m.getParameterTypes().length == 0 && clazz.isAssignableFrom(m.getReturnType()))

  protected def value[T](f: Field) = {
    f.setAccessible(true);
    val value = f.get(instance).asInstanceOf[T];
    value
  }
  lazy val values = fields.map(fieldMap)
  def findFieldWithValue(x: Any) = fields.find((f) => fieldMap(f) == x)
}

class ClassFieldSet[T](val instance: Any, clazz: Class[T]) extends Fields[T] {
  protected lazy val instantiateLazyVals: Unit = instantiateLazyVals(clazz)
  val filter = (f: Field) => clazz.isAssignableFrom(f.getType())
}

class AnnotatedFieldSet[T <: Annotation](val instance: Any, annotation: Class[T]) extends Fields[Any] {
  protected lazy val instantiateLazyVals: Unit = instantiateLazyVals((m: Method) => m.getAnnotation(annotation) != null)
  val filter = (f: Field) => f.getAnnotation(annotation) != null
}

object FieldSetPrinter {
  def printLine(printer: (Field, Any) => String, fieldSets: Fields[_]*): Seq[String] = {
    val attributeValuePairs = fieldSets.flatMap((fs)=> fs.fieldMap).sortBy{case (f,value) => f.getName().toUpperCase()}
    attributeValuePairs.map { case (field, value) => printer(field, value) }
  }
}

trait Structure[S, Result] extends HtmlDisplay {
  val annotatedFields = new AnnotatedFieldSet(this, classOf[Display])
  
  

  protected def findFragmentsToString(fragmentFieldMap: Map[Field, Fragment[S, Result, _, _]], endToString: (Result) => String, toStringFn: (Field, Any) => String = (f, a) => raw"${f.getName} = $a") =
    fragmentFieldMap.keys.toList.sortBy(_.getName).map(((f) => {
      val frag = fragmentFieldMap(f)
      val resultString = try {
        frag.convertor match {
          case Some(_) => frag.apply
          case _ => "No Convertor"
        }
      } catch { case e: Throwable => e.getClass.getSimpleName() + " " + e.getMessage }
      toStringFn(f, resultString)
    })).mkString("\n  ")

  protected def annotatedFieldsToString(toStringFn: (String, Any) => String) = {
    annotatedFields.fieldMap.keys.toList.sortBy(_.getName()).foldLeft("")((acc, f) => {
      val value = annotatedFields.fieldMap(f)
      acc + toStringFn(f.getName, value)
    })
  }

  protected def selfAndChildren(s: S, pathMap: PathMap[S, Result], separator: String = "\n"): (IndentAndString, List[Path]) => IndentAndString = (acc, p) => {
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
    val myString = acc.indent + p.mkString("") + valueString + separator
    val childrensString = pathMap(s, p).foldLeft(acc.indentedBlank)(selfAndChildren(s, pathMap))
    new IndentAndString(acc.indent, acc.string + myString + childrensString.string)
  }

  protected def structuresToString(pathMap: PathMap[S, Result], structureTitle: (S) => String, separator: String = "\n") =
    pathMap.roots.keys.toList.sortBy(structureTitle(_)).map((s) => structureTitle(s)
      + {
        val allPaths = pathMap.roots(s).flatMap((root) => Maps.walkSelfAndChildrenPaths(pathMap.map(s).map)(List(root)))
        allPaths.map((pathList) => {
          val last = pathList.last
          val fullPaths = pathMap.fullPaths(s)
          val fragments = fullPaths.filter(_.paths.reverse == last)
          val fragmentString = fragments.map((f) => try {
            f.convertor match {
              case Some(c) => f.apply
              case _ => "No Convertor"
            }
          } catch { case e: Throwable => e.getClass.getSimpleName }).mkString(",")
          val fullFragmentString = if (fragmentString.length() > 0) " = " + fragmentString else "";

          pathList.tail.map((x) => "..").mkString + last.last + fullFragmentString
        })
      }.mkString(separator)).mkString(separator)

  protected def structuresToStringOld(pathMap: PathMap[S, Result], structureTitle: (S) => String, separator: String = "\n") =
    pathMap.roots.keys.toList.sortBy(structureTitle(_)).map((s) => structureTitle(s) + separator
      + {
        pathMap.roots(s).foldLeft(IndentAndString("    ", ""))((acc, r) => selfAndChildren(s, pathMap, separator)(acc, List(r)))
      }.string).mkString(separator)

}
